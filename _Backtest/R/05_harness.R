# Walk-forward harness -------------------------------------------------------------
# For every game date in a season: fit on all games strictly before that date,
# predict that date's games. Nothing from the test date or later is used.

run_season <- function(model, league, season, priors = NULL,
                       start_date = NULL, cv_every = 7, verbose = TRUE) {
  summ <- load_summary(league, season)
  games <- games_from_summary(summ)
  dates <- sort(unique(games$game_date))
  if (!is.null(start_date)) dates <- dates[dates >= start_date]
  ctx <- list(league = league, season = season, priors = priors,
              cv_every = cv_every, lambda_cache = new.env())
  out <- vector("list", length(dates))
  for (i in seq_along(dates)) {
    d <- dates[i]
    train <- filter(summ, game_date < d)
    if (nrow(train) < 50 && is.null(priors)) next     # nothing to fit yet
    ctx$as_of <- d
    gd <- filter(games, game_date == d)
    st <- tryCatch(if (nrow(train) >= 50) model$fit(train, ctx) else NULL,
                   error = function(e) { message(d, ": ", conditionMessage(e)); NULL })
    if (is.null(st) && !is.null(priors) && nrow(train) < 50) {
      st <- if (!is.null(model$prior_state)) model$prior_state(priors) else prior_only_state(priors)  # opening days: priors alone
    }
    if (is.null(st)) next
    p <- model$predict(st, gd)
    out[[i]] <- p
    if (verbose && i %% 20 == 0) message(league, " ", season, " ", d, " (", i, "/", length(dates), ")")
  }
  preds <- bind_rows(out)
  games %>% inner_join(preds, by = "game_id") %>%
    mutate(league = league, season = season, model = model$label)
}

prior_only_state <- function(priors) lapply(priors[names(prod_stats)], coef_vec)

# Convenience: run a model over many seasons; priors = "prod" uses the saved
# production priors where they exist (else no prior), "none" never uses priors
run_model <- function(model, league, seasons, priors = c("prod", "none"),
                      cores = 2, ...) {
  priors <- match.arg(priors)
  if (.Platform$OS.type == "windows") cores <- 1   # mclapply cannot fork on Windows
  res <- parallel::mclapply(seasons, function(s) {
    pr <- if (priors == "prod") load_priors(league, s) else NULL
    run_season(model, league, s, priors = pr, verbose = FALSE, ...)
  }, mc.cores = cores)
  bad <- vapply(res, inherits, logical(1), "try-error")
  if (any(bad)) stop(paste(sapply(res[bad], as.character), collapse = "\n"))
  bind_rows(res)
}
