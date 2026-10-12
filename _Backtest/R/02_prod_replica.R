# Faithful replica of the production rating step -------------------------------
# create_ratings_prod() is a line-for-line copy of
#   _Helper Files/Function Scripts/create_ratings_stat.R
# (the serialized .rds production loads was checked against it).

create_ratings_prod <- function(df, stat_name, name_list, seed, wgt, lambda_adj,
                                standardize = TRUE, foldid = NULL) {
  model.data <- df %>%
    select(all_of(stat_name), all_of(name_list)) %>%
    group_by(team_id) %>% arrange(desc(game_date)) %>% ungroup() %>%
    mutate(team_id = as.character(team_id),
           opp_id = as.character(opp_id)) %>%
    group_by(team_id) %>%
    mutate(game = row_number(), weight = wgt ^ game) %>%
    ungroup() %>%
    arrange(game_date) %>%
    select(-c(game, game_date)) %>%
    fastDummies::dummy_cols(select_columns = c("team_id", "opp_id"),
                            remove_first_dummy = FALSE) %>%
    select(-c(team_id, opp_id)) %>%
    relocate(weight, .after = last_col()) %>%
    relocate(all_of(stat_name)) %>%
    as.data.frame()

  y <- model.data[, 1]
  x <- data.matrix(model.data[, 2:(length(model.data) - 1)])
  weight <- model.data$weight

  set.seed(seed)
  cv_model <- if (is.null(foldid)) {
    cv.glmnet(x, y, weights = weight, alpha = 0, standardize = standardize)
  } else {
    cv.glmnet(x, y, weights = weight, alpha = 0, standardize = standardize,
              foldid = foldid(df))
  }
  best_lambda <- cv_model$lambda.min * (1 + lambda_adj)
  best_model <- glmnet(x, y, weights = weight, alpha = 0, lambda = best_lambda,
                       standardize = standardize)

  raw.coeff <- as.data.frame(as.matrix(coef(best_model)))
  raw.coeff <- data.frame(name = rownames(raw.coeff), value = raw.coeff[[1]],
                          row.names = NULL)
  raw.coeff
}

# Games played per rating name (team_id_X / opp_id_X), as in production
games_played_tbl <- function(summary) {
  bind_rows(
    summary %>% mutate(name = paste0("team_id_", team_id)) %>%
      group_by(name) %>% summarise(games_played = n_distinct(game_id), .groups = "drop"),
    summary %>% mutate(name = paste0("opp_id_", opp_id)) %>%
      group_by(name) %>% summarise(games_played = n_distinct(game_id), .groups = "drop")
  )
}

# Prior blend, copied from the in-season script (efg_mbb_hardcode reproduces the
# MBB efg block's literal /30)
blend_with_prior <- function(curr, prior, games_played, wp, fixed_max = NULL) {
  max_games <- wp$max_games
  weight.df <- data.frame(game_no = seq_len(max_games)) %>%
    mutate(weight = wp$prior_wgt ^ game_no,
           weight = weight / sum(weight),
           cum_weight = cumsum(weight) * (1 - wp$prior_min)) %>%
    mutate(row = row_number()) %>% select(row, cum_weight)
  denom <- if (is.null(fixed_max)) max_games else fixed_max

  prior %>%
    full_join(curr, by = "name", suffix = c("_prior", "_curr")) %>%
    left_join(games_played, by = "name") %>%
    mutate(games_played = ifelse(is.na(games_played),
                                 round(mean(games_played, na.rm = TRUE), 2),
                                 games_played)) %>%
    left_join(weight.df, by = c("games_played" = "row")) %>%
    mutate(cum_weight = replace_na(cum_weight, max(cum_weight, na.rm = TRUE))) %>%
    mutate(value = case_when(
      is.na(value_curr) ~ value_prior,
      is.na(value_prior) ~ value_curr,
      str_detect(name, "team_id_") | str_detect(name, "opp_id_") ~
        cum_weight * value_curr + (1 - cum_weight) * value_prior,
      TRUE ~ (0.9 + 0.1 * games_played / denom) * value_curr +
        (0.1 - 0.1 * games_played / denom) * value_prior)) %>%
    select(name, value)
}

# Production in-season ratings as of `as_of` (uses games strictly before it)
# priors: production priors list, or NULL for the no-prior version
prod_ratings <- function(summary, league, season, as_of = NULL, priors = NULL,
                         stats = prod_stats, standardize = TRUE, foldid = NULL,
                         lambda_override = NULL) {
  train <- if (is.null(as_of)) summary else filter(summary, game_date < as_of)
  gp <- games_played_tbl(train)
  out <- list()
  for (nm in names(stats)) {
    st <- stats[[nm]]
    w <- load_weights(league, st, season)
    lam <- if (!is.null(lambda_override)) lambda_override[[nm]] else w$single$lambda_adj
    curr <- create_ratings_prod(train, st, prod_features[[st]], seed = 421,
                                wgt = w$single$wgt, lambda_adj = lam,
                                standardize = standardize, foldid = foldid)
    out[[nm]] <- if (is.null(priors)) curr else
      blend_with_prior(curr, priors[[nm]], gp, w$prior)
  }
  out
}
