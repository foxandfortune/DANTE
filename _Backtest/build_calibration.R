# Build the production calibration files ------------------------------------------------
# Usage (from the DANTE repo root):
#   Rscript _Backtest/run_variant.R C1_lam050 MBB   # walk-forward production fits (C1 weights), no stretch
#   Rscript _Backtest/run_variant.R C1_lam050 WBB
#   Rscript _Backtest/build_calibration.R
# Writes
#   VRGL/Stats/Team and Player Stats/Power Ratings/Weights/calibration_mbb.rds
#   BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights/calibration_wbb.rds
# which 4. Run Simulation / _March Madness Simulation load as ratings$calibration.
# Re-run each offseason after adding the finished season to the backtest.
# Options (environment variables):
#   BT_CAL_VARIANT=C1_lam050   build from another variant's predictions (default C1_lam050, production since 2026-10-24)
#   BT_CAL_OUT_DIR=_Backtest/output/candidate   write there instead of the production Weights folders

bt_dir <- if (dir.exists("_Backtest")) "_Backtest" else "/home/claude/_Backtest"
for (f in list.files(file.path(bt_dir, "R"), full.names = TRUE)) source(f)

out_path <- list(
  MBB = "VRGL/Stats/Team and Player Stats/Power Ratings/Weights/calibration_mbb.rds",
  WBB = "BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights/calibration_wbb.rds")
# The no-prior daily sims (inseason_ratings_all_no_prior) get their own
# calibration, fitted on the backtest seasons that had no production priors.
variant <- Sys.getenv("BT_CAL_VARIANT", "C1_lam050")
out_dir <- Sys.getenv("BT_CAL_OUT_DIR", "")
if (nzchar(out_dir)) out_path <- lapply(out_path, function(x) file.path(out_dir, basename(x)))
out_path_np <- lapply(out_path, function(x) sub("\\.rds$", "_no_prior.rds", x))

# Production runs with priors, so by default the calibration is fitted on the
# seasons that had production priors (they behave differently in Nov/Dec).
prior_seasons <- list(MBB = 2024:2099, WBB = 2025:2099)
# BT_CAL_PRIOR_FROM="MBB=2022,WBB=2024": first season to fit on, for a variant whose
# priors exist in more seasons (e.g. P_C1_pch, priors rebuilt for 2021+). The first
# season of such a run is left out, as in the backtest.
pf <- Sys.getenv("BT_CAL_PRIOR_FROM", "")
if (nzchar(pf)) for (kv in strsplit(strsplit(pf, ",")[[1]], "=")) prior_seasons[[kv[1]]] <- as.integer(kv[2]):2099
use_all <- identical(Sys.getenv("BT_CAL_ALL_SEASONS"), "1")

for (lg in c("MBB", "WBB")) {
  p <- load_variant(variant, lg, dir = file.path(bt_dir, "output"))
  if (is.null(p)) stop("Run run_variant.R ", variant, " ", lg, " first")
  p <- add_scores(p)
  h <- if (use_all) p else filter(p, season %in% prior_seasons[[lg]])
  cal <- fit_calibration(h, by_phase = TRUE, hfa = TRUE)
  cal$league <- lg
  cal$fitted_on_seasons <- sort(unique(h$season))
  cal$built <- as.character(Sys.Date())
  cal$source <- paste(variant, "walk-forward predictions (no stretch)")
  cat("\n==", lg, "calibration (seasons", paste(range(h$season), collapse = "-"), ")\n")
  cat("home-court correction:", round(cal$hfa, 2), "pts\n"); print(cal$phase_table)
  dir.create(dirname(out_path[[lg]]), showWarnings = FALSE, recursive = TRUE)
  saveRDS(cal, out_path[[lg]])
  cat("saved", out_path[[lg]], "\n")

  hn <- filter(p, !(season %in% prior_seasons[[lg]]))
  if (nzchar(pf)) { cat("-- no-prior calibration skipped (BT_CAL_PRIOR_FROM set; keep the existing file)\n"); next }
  caln <- fit_calibration(hn, by_phase = TRUE, hfa = TRUE)
  caln$league <- lg; caln$fitted_on_seasons <- sort(unique(hn$season))
  caln$built <- as.character(Sys.Date())
  caln$source <- paste(variant, "walk-forward predictions, seasons without priors (for no-prior sims)")
  cat("-- no-prior calibration (seasons", paste(range(hn$season), collapse = "-"), "): hfa", round(caln$hfa, 2), "\n")
  print(caln$phase_table)
  saveRDS(caln, out_path_np[[lg]])
  cat("saved", out_path_np[[lg]], "\n")
}
