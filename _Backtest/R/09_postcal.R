# Calibration layer -------------------------------------------------------------------
# fit_calibration(): estimates, from a set of historical predictions,
#   hfa         additive home-court correction (pts) for non-neutral home teams
#   phase_table per phase (Nov / Dec / Jan-Feb / Mar-Apr):
#               k          margin scale applied to the raw predicted margin
#               sd_margin  sd of margin around the calibrated prediction
#               total_bias added to the raw predicted total
#               sd_total   sd of total around the calibrated prediction
# The same object is what production's process_games_wk_calibrated() reads.

phase_levels <- c("1 Nov", "2 Dec", "3 Jan-Feb", "4 Mar-Apr")

fit_calibration <- function(h, by_phase = TRUE, hfa = TRUE) {
  h$g <- if (by_phase) h$phase else "all"
  hb <- 0
  if (hfa) {
    k0 <- unname(coef(lm(margin ~ 0 + pred_margin, h))[1])
    hb <- mean((h$margin - k0 * h$pred_margin)[!h$neutral])
  }
  tab <- h %>% group_by(g) %>% group_modify(function(d, ...) {
    adj <- ifelse(d$neutral, 0, hb)
    k <- unname(coef(lm(I(margin - adj) ~ 0 + pred_margin, d))[1])
    r <- d$margin - adj - k * d$pred_margin
    tb <- mean(d$total - d$pred_total)
    tibble(k = k, sd_margin = sd(r), total_bias = tb,
           sd_total = sd(d$total - d$pred_total - tb), n = nrow(d))
  }) %>% ungroup()
  if (!by_phase) tab <- tidyr::crossing(phase = phase_levels, tab %>% select(-g)) else
    tab <- tab %>% rename(phase = g)
  list(hfa = hb, phase_table = as.data.frame(tab))
}

apply_calibration <- function(p, cal) {
  p %>% select(-any_of(c("k", "sd_margin_c", "total_bias", "sd_total_c"))) %>%
    left_join(cal$phase_table %>% select(phase, k, sd_margin_c = sd_margin,
                                         total_bias, sd_total_c = sd_total), by = "phase") %>%
    mutate(pred_margin = k * pred_margin + ifelse(neutral, 0, cal$hfa),
           pred_total = pred_total + total_bias,
           mu_home = (pred_total + pred_margin) / 2,
           mu_away = (pred_total - pred_margin) / 2,
           sd_margin = sd_margin_c, sd_total = sd_total_c) %>%
    select(-k, -sd_margin_c, -total_bias, -sd_total_c)
}

strip_scores <- function(p) p %>%
  select(-any_of(c("p_home", "err_margin", "err_total", "err_home", "err_away",
                   "logloss", "brier", "z_margin", "z_total", "home_win")))

# Walk-forward: season s is calibrated with parameters fitted on seasons < s
# (optionally only on seasons that had production priors, `use_seasons`)
calibrate_layers <- function(p, by_phase = TRUE, hfa = TRUE, min_hist = 1,
                             hist_filter = NULL) {
  seasons <- sort(unique(p$season))
  bind_rows(lapply(seasons, function(s) {
    h <- filter(p, season < s)
    if (!is.null(hist_filter)) h <- hist_filter(h)
    if (length(unique(h$season)) < min_hist) return(NULL)
    cal <- fit_calibration(h, by_phase, hfa)
    apply_calibration(strip_scores(filter(p, season == s)), cal)
  })) %>% select(-any_of("phase")) %>% add_scores()
}
