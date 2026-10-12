# Reporting helpers -------------------------------------------------------------------

# Honest, walk-forward calibration across seasons: for season s, the margin
# scale (slope of actual on predicted margin), margin sd, total bias and total sd
# are estimated only from seasons before s. The first season has no history and
# is dropped from calibrated results.
calibrate_by_season <- function(p) {
  seasons <- sort(unique(p$season))
  bind_rows(lapply(seasons[-1], function(s) {
    h <- filter(p, season < s); cur <- filter(p, season == s)
    k   <- unname(coef(lm(margin ~ 0 + pred_margin, h))[1])
    sdm <- sd(h$margin - k * h$pred_margin)
    tb  <- mean(h$total - h$pred_total)
    sdt <- sd(h$total - h$pred_total - tb)
    cur %>% mutate(pred_margin = k * pred_margin, pred_total = pred_total + tb,
                   mu_home = (pred_total + pred_margin) / 2,
                   mu_away = (pred_total - pred_margin) / 2,
                   sd_margin = sdm, sd_total = sdt, cal_k = k)
  })) %>%
    select(-any_of(c("p_home", "err_margin", "err_total", "err_home", "err_away",
                     "logloss", "brier", "z_margin", "z_total", "home_win", "phase"))) %>%
    add_scores()
}

load_variant <- function(v, lg, dir = if (dir.exists("_Backtest/output")) "_Backtest/output" else "/home/claude/_Backtest/output") {
  f <- file.path(dir, glue::glue("pred_{v}_{lg}.rds"))
  if (!file.exists(f)) return(NULL)
  readRDS(f) %>% mutate(model = v)
}

# Restrict a list of prediction frames to games every model predicted
common_games <- function(lst) {
  ids <- Reduce(intersect, lapply(lst, function(d) d$game_id))
  lapply(lst, function(d) filter(d, game_id %in% ids))
}
