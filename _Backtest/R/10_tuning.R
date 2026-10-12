# One-change-at-a-time tuning ------------------------------------------------------------
# For one target component, every config in `configs` changes ONE setting of that
# component (decay `wgt` or `lambda_mult`, or CV folds by game) while all other
# components keep their production settings. The non-target components are fit
# once per date and shared by every config, so a whole grid costs roughly
# (4 + n_configs) ridge fits per date.
#
# configs: named list of list(wgt =, lambda_mult =, fold_by_game =); NULL fields
#          mean "production value". The "prod" config must be included.

tune_grid <- function(base, wgts = NULL, lambda_mults = NULL, fold_by_game = TRUE) {
  cfg <- list(prod = list())
  for (w in wgts) cfg[[sprintf("wgt=%.3f", w)]] <- list(wgt = w)
  for (l in lambda_mults) cfg[[sprintf("lam_x%.2f", l)]] <- list(lambda_mult = l)
  if (fold_by_game) cfg[["fold_by_game"]] <- list(fold_by_game = TRUE)
  cfg
}

tune_stat_season <- function(league, season, target, configs, cv_every = 7, last_n_dates = NULL) {
  summ <- load_summary(league, season)
  games <- games_from_summary(summ)
  dates <- sort(unique(games$game_date))
  if (!is.null(last_n_dates)) dates <- tail(dates, last_n_dates)   # for quick tests
  ctx <- list(league = league, season = season, cv_every = cv_every,
              lambda_cache = new.env())
  predictor <- model_components(stretch = 1, label = "tune")$predict
  prod_w <- lapply(prod_stats, function(st) load_weights(league, st, season))
  out <- list()
  for (d in as.list(dates)) {
    train <- filter(summ, game_date < d)
    if (nrow(train) < 50) next
    ctx$as_of <- d
    gd <- filter(games, game_date == d)
    fixed <- list()
    for (nm in setdiff(names(prod_stats), target)) {
      st <- prod_stats[[nm]]; w <- prod_w[[nm]]$single
      fixed[[nm]] <- coef_vec(cached_ridge(ctx, paste("fixed", nm), train, st,
                                           prod_features[[st]], w$wgt,
                                           lambda_mult = 1 + w$lambda_adj))
    }
    st <- prod_stats[[target]]; w <- prod_w[[target]]$single
    for (cn in names(configs)) {
      c0 <- configs[[cn]]
      wg <- if (is.null(c0$wgt)) w$wgt else c0$wgt
      lm <- if (is.null(c0$lambda_mult)) 1 + w$lambda_adj else c0$lambda_mult
      fg <- isTRUE(c0$fold_by_game)
      cf <- tryCatch(coef_vec(cached_ridge(ctx, paste("target", cn), train, st,
                                           prod_features[[st]], wg, lambda_mult = lm,
                                           fold_by_game = fg)),
                     error = function(e) NULL)
      if (is.null(cf)) next
      state <- fixed; state[[target]] <- cf
      out[[length(out) + 1]] <- predictor(state[names(prod_stats)], gd) %>%
        mutate(config = cn)
    }
  }
  games %>% inner_join(bind_rows(out), by = "game_id") %>%
    mutate(league = league, season = season, target = target)
}

# Evaluate configs: each config's predictions are calibrated walk-forward
# (phase scale/sd + HFA, from earlier tuning seasons) and compared, on
# identical games, with the production config.
evaluate_tuning <- function(res, B = 500) {
  cfgs <- unique(res$config)
  lst <- lapply(cfgs, function(cn) add_scores(filter(res, config == cn)))
  names(lst) <- cfgs
  lst <- common_games(lst)
  cal <- lapply(lst, calibrate_layers)
  bind_rows(lapply(setdiff(cfgs, "prod"), function(cn) {
    bind_rows(lapply(c("sq_margin", "logloss", "sq_total"), function(m)
      compare_models(cal[["prod"]], cal[[cn]], m, B = B))) %>% mutate(config = cn)
  })) %>% mutate(target = unique(res$target), league = unique(res$league))
}
