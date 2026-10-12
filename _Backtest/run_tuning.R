# One-change-at-a-time tuning runner ------------------------------------------------------
# Usage (from the DANTE repo root):  Rscript _Backtest/run_tuning.R <MBB|WBB> <component>
#   component: pace | ast | oreb | to | rtg
# Tuning seasons: MBB 2019-2023, WBB 2022-2024 (2024+/2025+ are held out for
# validation of the combined winners). Writes output/tune_<league>_<component>.rds
# and output/tune_eval_<league>_<component>.csv.

bt_dir <- if (dir.exists("_Backtest")) "_Backtest" else "/home/claude/_Backtest"
for (f in list.files(file.path(bt_dir, "R"), full.names = TRUE)) source(f)

args <- commandArgs(TRUE); lg <- args[1]; target <- args[2]
tune_seasons <- list(MBB = 2019:2023, WBB = 2022:2024)[[lg]]

# Grid around the production value (production weights for the nearest season)
w <- load_weights(lg, prod_stats[[target]], max(tune_seasons))$single
wgts <- sort(unique(pmin(1, round(w$wgt + c(-0.06, -0.03, 0.01, 0.02), 3))))
wgts <- setdiff(wgts, w$wgt)
lams <- setdiff(c(0.5, 1, 1.5, 2, 3), 1 + w$lambda_adj)
configs <- tune_grid(w, wgts = wgts, lambda_mults = lams, fold_by_game = TRUE)
cat(lg, target, "production: wgt", w$wgt, "lambda_mult", 1 + w$lambda_adj,
    "| configs:", paste(names(configs), collapse = ", "), "\n")

t0 <- Sys.time()
cores <- if (.Platform$OS.type == "windows") 1 else 2
res <- bind_rows(parallel::mclapply(tune_seasons, function(s)
  tune_stat_season(lg, s, target, configs), mc.cores = cores))
saveRDS(res, file.path(bt_dir, "output", glue::glue("tune_{lg}_{target}.rds")))
ev <- evaluate_tuning(res)
write.csv(ev, file.path(bt_dir, "output", glue::glue("tune_eval_{lg}_{target}.csv")), row.names = FALSE)
print(ev %>% select(config, metric, diff, lo95, hi95), n = 100)
cat("done in", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "min\n")
