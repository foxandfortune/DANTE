# Metrics ---------------------------------------------------------------------------

add_scores <- function(p) {
  p %>% filter(!is.na(mu_home), !is.na(mu_away), !is.na(sd_margin)) %>% mutate(
    pred_margin = mu_home - mu_away,
    pred_total  = mu_home + mu_away,
    p_home      = pnorm(pred_margin / sd_margin),
    p_home      = pmin(pmax(p_home, 1e-4), 1 - 1e-4),
    home_win    = as.numeric(margin > 0),
    err_margin  = margin - pred_margin,
    err_total   = total - pred_total,
    err_home    = home_score - mu_home,
    err_away    = away_score - mu_away,
    logloss     = -(home_win * log(p_home) + (1 - home_win) * log(1 - p_home)),
    brier       = (p_home - home_win)^2,
    # standardized residuals: should have sd ~1 if the stated uncertainty is right
    z_margin    = err_margin / sd_margin,
    z_total     = err_total / sd_total,
    phase = case_when(as.numeric(format(game_date, "%m")) == 11 ~ "1 Nov",
                      as.numeric(format(game_date, "%m")) == 12 ~ "2 Dec",
                      as.numeric(format(game_date, "%m")) %in% 1:2 ~ "3 Jan-Feb",
                      TRUE ~ "4 Mar-Apr"))
}

summarise_metrics <- function(p, ...) {
  p %>% group_by(...) %>% summarise(
    n = n(),
    margin_mae = mean(abs(err_margin)), margin_rmse = sqrt(mean(err_margin^2)),
    margin_bias = mean(err_margin),
    margin_slope = coef(lm(margin ~ pred_margin))[2],
    logloss = mean(logloss), brier = mean(brier),
    total_mae = mean(abs(err_total)), total_rmse = sqrt(mean(err_total^2)),
    total_bias = mean(err_total),
    total_slope = coef(lm(total ~ pred_total))[2],
    team_pts_rmse = sqrt(mean(c(err_home, err_away)^2)),
    z_margin_sd = sd(z_margin), z_total_sd = sd(z_total),
    .groups = "drop")
}

# Paired comparison of model B vs model A on identical games; bootstrap resamples
# whole game dates (games on the same day share information)
compare_models <- function(pa, pb, metric = c("abs_margin", "sq_margin", "logloss",
                                               "brier", "abs_total", "sq_total"),
                           B = 2000, seed = 1) {
  metric <- match.arg(metric)
  f <- switch(metric,
              abs_margin = function(d) abs(d$err_margin),
              sq_margin  = function(d) d$err_margin^2,
              logloss    = function(d) d$logloss,
              brier      = function(d) d$brier,
              abs_total  = function(d) abs(d$err_total),
              sq_total   = function(d) d$err_total^2)
  j <- inner_join(pa %>% transmute(game_id, game_date, a = f(pa)),
                  pb %>% transmute(game_id, b = f(pb)), by = "game_id") %>%
    mutate(diff = b - a)
  by_date <- j %>% group_by(game_date) %>% summarise(s = sum(diff), n = n(), .groups = "drop")
  set.seed(seed)
  boots <- replicate(B, { k <- sample(nrow(by_date), replace = TRUE)
    sum(by_date$s[k]) / sum(by_date$n[k]) })
  tibble(metric = metric, n = nrow(j), mean_a = mean(j$a), mean_b = mean(j$b),
         diff = mean(j$diff), lo95 = unname(quantile(boots, .025)),
         hi95 = unname(quantile(boots, .975)))
}

calibration_table <- function(p, bins = seq(0, 1, .1)) {
  p %>% mutate(bin = cut(p_home, bins, include.lowest = TRUE)) %>%
    group_by(bin) %>% summarise(n = n(), pred = mean(p_home), obs = mean(home_win),
                                .groups = "drop")
}
