# Post-hoc calibration of production (V0) predictions -------------------------------
# Usage (from the DANTE repo root): Rscript _Backtest/calibration_layers.R
# Needs output/pred_V0_prod_<league>.rds from run_variant.R.
bt_dir <- if (dir.exists("_Backtest")) "_Backtest" else "/home/claude/_Backtest"
for (f in list.files(file.path(bt_dir, "R"), full.names = TRUE)) source(f)
options(width = 200, pillar.sigfig = 4)
for (lg in c("MBB", "WBB")) {
  p <- add_scores(load_variant("V0_prod", lg, dir = file.path(bt_dir, "output")))
  first <- min(p$season)
  versions <- common_games(list(
    asis           = filter(p, season > first),
    cal_global     = calibrate_by_season(p),
    cal_phase      = calibrate_layers(p, by_phase = TRUE,  hfa = FALSE),
    cal_global_hfa = calibrate_layers(p, by_phase = FALSE, hfa = TRUE),
    cal_phase_hfa  = calibrate_layers(p, by_phase = TRUE,  hfa = TRUE)))
  cat("=====", lg, "production predictions + calibration layers (seasons >", first, ")\n")
  tab <- bind_rows(lapply(names(versions), function(v) summarise_metrics(versions[[v]]) %>% mutate(version = v)))
  print(tab %>% select(version, n, margin_mae, margin_rmse, margin_slope, logloss, brier,
                       total_mae, total_rmse, z_margin_sd, z_total_sd))
  cmp <- bind_rows(lapply(names(versions)[-1], function(v)
    bind_rows(lapply(c("sq_margin", "logloss", "sq_total"), function(m)
      compare_models(versions$asis, versions[[v]], m, B = 1000))) %>% mutate(version = v)))
  print(cmp %>% select(version, metric, diff, lo95, hi95))
  write.csv(tab, file.path(bt_dir, "output", glue::glue("calibration_layers_{lg}.csv")), row.names = FALSE)
  write.csv(cmp, file.path(bt_dir, "output", glue::glue("calibration_layers_paired_{lg}.csv")), row.names = FALSE)
}
