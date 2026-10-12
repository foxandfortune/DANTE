# Expected rating + team shooting (X models) vs production, both calibrated --------------
# Usage (from the DANTE repo root): Rscript _Backtest/compare_xshoot.R
# Both sides get the same walk-forward calibration layer (phase scale/sd + HFA),
# so differences come from the rating model only. Results are split into
# seasons without priors (clean: neither model has a prior) and with priors
# (the X model borrows the rtg prior for its expected-rating part).

bt_dir <- if (dir.exists("_Backtest")) "_Backtest" else "/home/claude/_Backtest"
for (f in list.files(file.path(bt_dir, "R"), full.names = TRUE)) source(f)
options(width = 220, pillar.sigfig = 4)
xs <- c("X1_xshoot", "X2_xshoot_fast", "X3_xshoot_heavy", "X4_xshoot_impute", "X5_impute_light", "X6_impute_vlight")
prior_from <- c(MBB = 2024, WBB = 2025)

for (lg in c("MBB", "WBB")) {
  vs <- c("V0_raw", xs)
  raw <- Filter(Negate(is.null), setNames(lapply(vs, load_variant, lg = lg), vs))
  raw <- common_games(lapply(raw, add_scores))
  cal <- lapply(raw, calibrate_layers)
  cat("\n=================", lg, "| games", nrow(cal[[1]]), "=================\n")
  for (era in c("no priors", "with priors", "all")) {
    sel <- function(d) switch(era, "no priors" = filter(d, season < prior_from[[lg]]),
                              "with priors" = filter(d, season >= prior_from[[lg]]), all = d)
    cs <- lapply(cal, sel)
    cat("\n--", era, "seasons:", paste(range(cs[[1]]$season), collapse = "-"), "--\n")
    print(bind_rows(lapply(names(cs), function(v) summarise_metrics(cs[[v]]) %>% mutate(model = v))) %>%
            select(model, n, margin_mae, margin_rmse, logloss, total_mae, total_rmse, team_pts_rmse))
    print(bind_rows(lapply(setdiff(names(cs), "V0_raw"), function(v)
      bind_rows(lapply(c("sq_margin", "logloss", "sq_total"), function(m)
        compare_models(cs[["V0_raw"]], cs[[v]], m, B = 1000))) %>% mutate(model = v))) %>%
        select(model, metric, diff, lo95, hi95))
  }
  lines_dir <- Sys.getenv("BT_LINES_DIR", bt_path("MBB", "results", season = 2026))
  if (lg == "MBB" && dir.exists(lines_dir)) {
    L <- load_saved_lines(lg, 2026, lines_dir)
    cat("\n-- 2026 vs Vegas morning consensus --\n")
    print(bind_rows(lapply(names(cal), function(v) {
      d <- cal[[v]] %>% filter(season == 2026)
      s <- bind_rows(d %>% inner_join(L$spread, by = c("game_id", "home_id" = "team_id")) %>% mutate(vm = -spread),
                     d %>% inner_join(L$spread, by = c("game_id", "away_id" = "team_id")) %>% mutate(vm = spread)) %>%
        distinct(game_id, .keep_all = TRUE)
      t <- d %>% inner_join(L$total, by = "game_id")
      tibble(model = v, spread_mae = mean(abs(s$err_margin)), vegas_spread_mae = mean(abs(s$margin - s$vm)),
             spread_info = cor(s$pred_margin - s$vm, s$margin - s$vm),
             total_mae = mean(abs(t$err_total)), vegas_total_mae = mean(abs(t$total - t$vegas_total)),
             total_info = cor(t$pred_total - t$vegas_total, t$total - t$vegas_total))
    })))
  }
}
