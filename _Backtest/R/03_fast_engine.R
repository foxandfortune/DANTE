# Fast rating engine -------------------------------------------------------------
# Same model as create_ratings_prod() but with a sparse design matrix, so the
# walk-forward backtest (one fit per game date) is practical. Row order, recency
# weights, CV fold assignment (set.seed(421) + glmnet's default sampling) and
# lambda rule are identical; tested against the production replica.
#
# Options used by experiments:
#   standardize  : glmnet standardize flag (production = TRUE)
#   fold_by_game : CV folds assigned per game_id instead of per row
#   lambda_mult  : lambda = lambda.min * lambda_mult (production = 1 + lambda_adj)
#   offset_prior : named vector of prior coefficients for team/opp dummies; the
#                  model is fit on y - offset and shrinks toward the prior
#   fixed_lambda : skip CV and use this lambda

prep_design <- function(df, stat_name, name_list, wgt) {
  d <- df %>%
    select(all_of(c(stat_name, name_list, "game_id"))) %>%
    group_by(team_id) %>% arrange(desc(game_date)) %>% ungroup() %>%
    group_by(team_id) %>% mutate(game = row_number(), weight = wgt ^ game) %>%
    ungroup() %>%
    arrange(game_date)

  tm <- paste0("team_id_", d$team_id)
  op <- paste0("opp_id_", d$opp_id)
  # fastDummies orders levels by sorted character value
  tlev <- paste0("team_id_", sort(unique(as.character(d$team_id))))
  olev <- paste0("opp_id_", sort(unique(as.character(d$opp_id))))
  n <- nrow(d)
  extra <- setdiff(name_list, c("game_date", "team_id", "opp_id"))
  Xe <- if (length(extra)) as.matrix(sapply(d[extra], as.numeric)) else NULL
  Xt <- sparseMatrix(i = seq_len(n), j = match(tm, tlev), x = 1,
                     dims = c(n, length(tlev)), dimnames = list(NULL, tlev))
  Xo <- sparseMatrix(i = seq_len(n), j = match(op, olev), x = 1,
                     dims = c(n, length(olev)), dimnames = list(NULL, olev))
  X <- if (is.null(Xe)) cbind(Xt, Xo) else cbind(Matrix(Xe, sparse = TRUE), Xt, Xo)
  if (!is.null(Xe)) colnames(X)[seq_along(extra)] <- extra
  list(X = X, y = d[[stat_name]], w = d$weight, game_id = d$game_id,
       team = tm, opp = op)
}

fit_ridge <- function(df, stat_name, name_list, wgt, lambda_mult = 1,
                      standardize = TRUE, fold_by_game = FALSE,
                      offset_prior = NULL, fixed_lambda = NULL, seed = 421,
                      netlevel_ratio = NULL) {
  D <- prep_design(df, stat_name, name_list, wgt)
  # Net/level parameterization (optional): team offense O and defense D are
  # rewritten as O = net + level, D = level - net, so a game row (offense i vs
  # defense j) has net_i - net_j + level_i + level_j. `net` drives margins and
  # `level` drives totals; level columns get netlevel_ratio x the penalty.
  # Coefficients are converted back to team_id_/opp_id_ form on output.
  if (!is.null(netlevel_ratio)) {
    ids <- sort(unique(c(sub("^team_id_", "", D$team), sub("^opp_id_", "", D$opp))))
    n <- length(D$y); oi <- match(sub("^team_id_", "", D$team), ids)
    dj <- match(sub("^opp_id_", "", D$opp), ids)
    Xn <- sparseMatrix(i = c(seq_len(n), seq_len(n)), j = c(oi, dj), x = c(rep(1, n), rep(-1, n)),
                       dims = c(n, length(ids)), dimnames = list(NULL, paste0("net_", ids)))
    Xl <- sparseMatrix(i = c(seq_len(n), seq_len(n)), j = c(oi, dj), x = 1,
                       dims = c(n, length(ids)), dimnames = list(NULL, paste0("lvl_", ids)))
    extra <- colnames(D$X)[!grepl("^(team_id_|opp_id_)", colnames(D$X))]
    D$X <- if (length(extra)) cbind(D$X[, extra, drop = FALSE], Xn, Xl) else cbind(Xn, Xl)
  }
  off <- rep(0, length(D$y))
  if (!is.null(offset_prior)) {
    pt <- offset_prior[D$team]; pt[is.na(pt)] <- 0
    po <- offset_prior[D$opp];  po[is.na(po)] <- 0
    off <- pt + po
  }
  yy <- D$y - off
  # Without column standardization, glmnet's automatic lambda path is driven by
  # the large-scale covariates (travel in miles) and shrinks team effects to ~0.
  # So when standardize = FALSE: situational / component covariates are left
  # unpenalized (fixed effects), only team dummies are penalized (random-effect
  # analogue), and an explicit lambda grid sized for 0/1 dummies is used.
  pf <- rep(1, ncol(D$X))
  if (!is.null(netlevel_ratio)) pf[grepl("^lvl_", colnames(D$X))] <- netlevel_ratio
  lam_grid <- NULL
  if (!standardize) {
    pf[!grepl("^(team_id_|opp_id_)", colnames(D$X))] <- 0
    lam_grid <- exp(seq(log(20), log(1e-6), length.out = 100))
  }
  if (is.null(fixed_lambda)) {
    set.seed(seed)
    foldid <- if (fold_by_game) {
      g <- unique(D$game_id); f <- sample(rep(1:10, length = length(g)))
      f[match(D$game_id, g)]
    } else sample(rep(1:10, length = length(yy)))
    if (!standardize) {            # keep every fold on the full lambda grid
      old_ctl <- glmnet.control()
      on.exit(glmnet.control(fdev = old_ctl$fdev, devmax = old_ctl$devmax), add = TRUE)
      glmnet.control(fdev = 0, devmax = 1)
    }
    cv <- cv.glmnet(D$X, yy, weights = D$w, alpha = 0,
                    standardize = standardize, foldid = foldid,
                    penalty.factor = pf, lambda = lam_grid)
    if (!standardize && cv$lambda.min %in% range(lam_grid))
      warning(stat_name, ": lambda.min on grid boundary")
    lam <- cv$lambda.min * lambda_mult
  } else lam <- fixed_lambda
  m <- glmnet(D$X, yy, weights = D$w, alpha = 0, lambda = lam,
              standardize = standardize, penalty.factor = pf)
  cf <- as.matrix(coef(m))
  out <- data.frame(name = rownames(cf), value = cf[, 1], row.names = NULL)
  if (!is.null(netlevel_ratio)) {
    v <- setNames(out$value, out$name)
    ids <- sub("^net_", "", grep("^net_", out$name, value = TRUE))
    nt <- v[paste0("net_", ids)]; lv <- v[paste0("lvl_", ids)]
    out <- bind_rows(out %>% filter(!grepl("^(net_|lvl_)", name)),
                     data.frame(name = paste0("team_id_", ids), value = unname(nt + lv)),
                     data.frame(name = paste0("opp_id_", ids), value = unname(lv - nt)))
  }
  if (!is.null(offset_prior)) {
    add <- offset_prior[out$name]; add[is.na(add)] <- 0
    out$value <- out$value + add
  }
  attr(out, "lambda") <- lam
  out
}
