# Compare experiment variants on identical games ------------------------------------
# Usage (from the DANTE repo root):  Rscript _Backtest/compare_variants.R
# Reads _Backtest/output/pred_<variant>_<league>.rds, writes
# _Backtest/output/comparison_<league>.csv and prints summaries.

bt_dir <- if (dir.exists("_Backtest")) "_Backtest" else "/home/claude/_Backtest"
for (f in list.files(file.path(bt_dir, "R"), full.names = TRUE)) source(f)
options(width = 220, pillar.sigfig = 4, dplyr.summarise.inform = FALSE)

vnames <- c("V0_prod", "V1_sitcomp", "V2_ridgefix", "D0_direct", "D1_directfix")
extra <- strsplit(Sys.getenv("BT_EXTRA_VARIANTS", ""), ",")[[1]]
vnames <- unique(c(vnames, extra[extra != ""]))

for (lg in c("MBB", "WBB")) {
  raw <- Filter(Negate(is.null), setNames(lapply(vnames, load_variant, lg = lg), vnames))
  if (!length(raw)) next
  raw <- lapply(raw, add_scores)
  raw <- common_games(raw)
  cal <- lapply(raw, calibrate_by_season)

  cat("\n=================", lg, "=================\n")
  cat("Games (common to all variants):", nrow(raw[[1]]), "| calibrated seasons:",
      paste(range(cal[[1]]$season), collapse = "-"), "\n")

  cat("\n-- As-is (each model's own stretch and sigma) --\n")
  asis <- bind_rows(lapply(names(raw), function(v) summarise_metrics(raw[[v]]) %>% mutate(model = v)))
  print(asis %>% select(model, n, margin_mae, margin_rmse, margin_bias, margin_slope, logloss,
                        total_mae, total_rmse, total_bias, z_margin_sd, z_total_sd))

  cat("\n-- Calibrated walk-forward across seasons (scale/sd/total bias from prior seasons only) --\n")
  calm <- bind_rows(lapply(names(cal), function(v) summarise_metrics(cal[[v]]) %>% mutate(model = v)))
  print(calm %>% select(model, n, margin_mae, margin_rmse, margin_slope, logloss, brier,
                        total_mae, total_rmse, team_pts_rmse, z_margin_sd, z_total_sd))

  cat("\n-- Calibrated, by phase: margin RMSE / logloss --\n")
  print(bind_rows(lapply(names(cal), function(v) summarise_metrics(cal[[v]], phase) %>% mutate(model = v))) %>%
          select(model, phase, n, margin_rmse, logloss, total_rmse) %>%
          pivot_wider(names_from = phase, values_from = c(margin_rmse, logloss, total_rmse), id_cols = model))

  cat("\n-- Paired differences vs V0_prod (calibrated; negative = better; 95% CI by date bootstrap) --\n")
  cmp <- bind_rows(lapply(setdiff(names(cal), "V0_prod"), function(v)
    bind_rows(lapply(c("sq_margin", "abs_margin", "logloss", "sq_total"), function(m)
      compare_models(cal[["V0_prod"]], cal[[v]], m, B = 1000))) %>% mutate(model = v)))
  print(cmp %>% select(model, metric, n, mean_a, mean_b, diff, lo95, hi95))

  cat("\n-- Home-court check: mean (actual - predicted) home margin, non-neutral, calibrated --\n")
  print(bind_rows(lapply(names(cal), function(v) cal[[v]] %>% filter(!neutral) %>%
    summarise(model = v, n = n(), home_bias = mean(err_margin), se = sd(err_margin) / sqrt(n())))))

  write.csv(bind_rows(asis %>% mutate(mode = "as_is"), calm %>% mutate(mode = "calibrated")),
            file.path(bt_dir, "output", glue::glue("comparison_{lg}.csv")), row.names = FALSE)
  write.csv(cmp, file.path(bt_dir, "output", glue::glue("paired_vs_prod_{lg}.csv")), row.names = FALSE)

  # Vegas benchmark (MBB 2026 only: morning consensus lines saved by the sim)
  lines_dir <- Sys.getenv("BT_LINES_DIR", bt_path("MBB", "results", season = 2026))
  if (lg == "MBB" && dir.exists(lines_dir)) {
    L <- load_saved_lines(lg, 2026, lines_dir)
    vg <- bind_rows(lapply(names(cal), function(v) {
      d <- cal[[v]] %>% filter(season == 2026)
      s1 <- d %>% inner_join(L$spread, by = c("game_id", "home_id" = "team_id")) %>% mutate(vm = -spread)
      s2 <- d %>% inner_join(L$spread, by = c("game_id", "away_id" = "team_id")) %>% mutate(vm = spread)
      s <- bind_rows(s1, s2) %>% distinct(game_id, .keep_all = TRUE)
      t <- d %>% inner_join(L$total, by = "game_id")
      tibble(model = v, n_spread = nrow(s),
             model_mae = mean(abs(s$err_margin)), vegas_mae = mean(abs(s$margin - s$vm)),
             info_corr = cor(s$pred_margin - s$vm, s$margin - s$vm),
             n_total = nrow(t), model_total_mae = mean(abs(t$err_total)),
             vegas_total_mae = mean(abs(t$total - t$vegas_total)),
             total_info_corr = cor(t$pred_total - t$vegas_total, t$total - t$vegas_total))
    }))
    cat("\n-- 2026 vs Vegas morning consensus (calibrated) --\n"); print(vg)
    write.csv(vg, file.path(bt_dir, "output", "vegas_2026_MBB.csv"), row.names = FALSE)
  }
}
