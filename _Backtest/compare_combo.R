# Combined tuning winners vs production, validated on held-out seasons --------------
# Usage (from the DANTE repo root): Rscript _Backtest/compare_combo.R
# Every model gets the same walk-forward calibration layer (phase scale/sd +
# home-court, fitted on earlier seasons only). The tuning grids used MBB
# 2019-2023 and WBB 2022-2024, so the held-out seasons (MBB 2024-2026, WBB
# 2025-2026) are the honest test; tuning-season results are shown for context.

bt_dir <- if (dir.exists("_Backtest")) "_Backtest" else "/home/claude/_Backtest"
for (f in list.files(file.path(bt_dir, "R"), full.names = TRUE)) source(f)
options(width = 220, pillar.sigfig = 4)
vs <- c("V0_raw", "C1_lam050", "C3_lam050_nl4", "C2_lam025", "C4_lam025_nl8")
holdout_from <- c(MBB = 2024, WBB = 2025)
out <- list()

for (lg in c("MBB", "WBB")) {
  raw <- Filter(Negate(is.null), setNames(lapply(vs, load_variant, lg = lg), vs))
  raw <- common_games(lapply(raw, add_scores))
  cal <- lapply(raw, calibrate_layers)
  cat("\n=================", lg, "=================\n")
  for (era in c("held-out", "tuning")) {
    cs <- lapply(cal, function(d) if (era == "held-out") filter(d, season >= holdout_from[[lg]])
                                  else filter(d, season < holdout_from[[lg]]))
    cat("\n--", era, "seasons", paste(range(cs[[1]]$season), collapse = "-"), "| games", nrow(cs[[1]]), "--\n")
    m <- bind_rows(lapply(names(cs), function(v) summarise_metrics(cs[[v]]) %>% mutate(model = v)))
    print(m %>% select(model, n, margin_mae, margin_rmse, logloss, brier, total_mae, total_rmse, team_pts_rmse))
    pc <- bind_rows(lapply(setdiff(names(cs), "V0_raw"), function(v)
      bind_rows(lapply(c("sq_margin", "logloss", "sq_total"), function(k)
        compare_models(cs[["V0_raw"]], cs[[v]], k, B = 1000))) %>% mutate(model = v)))
    print(pc %>% select(model, metric, diff, lo95, hi95))
    out[[paste(lg, era)]] <- list(metrics = m %>% mutate(league = lg, era = era),
                                  paired = pc %>% mutate(league = lg, era = era))
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
write.csv(bind_rows(lapply(out, `[[`, "metrics")), file.path(bt_dir, "output", "combo_metrics.csv"), row.names = FALSE)
write.csv(bind_rows(lapply(out, `[[`, "paired")), file.path(bt_dir, "output", "combo_paired.csv"), row.names = FALSE)
