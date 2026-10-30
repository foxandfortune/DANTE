# ──────────────────────────────────────────────────────────────────────────────
# 7. Rebuild Calibration.R  (both leagues)
#
# Re-fits the calibration the daily sims use (margin scale, margin/total spread,
# totals bias by month, home-court correction) with the season that just finished
# added. Run it after steps 3, 5 and 6.
#
# How:
#   a. Rebuild honest priors for every past season with the current prior models
#      (each fit only on earlier seasons), as 4. Create Priors would have made them.
#   b. Walk-forward backtest of production as configured (variant PROD: shrinkage,
#      decays and prior blend read from the weight files) with those priors, plus
#      a no-prior run for the no-prior sims.
#   c. Fit the calibration on those predictions and save it to the Weights folders.
#      The old files are copied to _Offseason Update/backup/<date>/ first.
#
# Takes roughly 1-1.5 hours on Windows (one core). Set RUN_NO_PRIOR <- FALSE to
# skip the no-prior refit (about a third of the time) and keep the current one.
# ──────────────────────────────────────────────────────────────────────────────
RUN_NO_PRIOR <- TRUE

# Find the DANTE folder: from this script's location when it is open in RStudio,
# else from the working directory (Rscript, GitHub Actions). The DANTE project
# doesn't need to be open.
dante_root <- local({
  starts <- c(tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) ""), getwd())
  for (s in starts[nzchar(starts)]) {
    r <- tryCatch(rprojroot::find_root(rprojroot::has_file("DANTE.Rproj"), path = s), error = function(e) NULL)
    if (!is.null(r)) return(r)
  }
  stop("Can't find the DANTE folder (DANTE.Rproj): open the DANTE project or setwd() into it")
})
dante_path <- function(...) file.path(dante_root, ...)
setwd(dante_path())
source(dante_path("_Helper Files", "season.R"))
for (f in list.files("_Backtest/R", full.names = TRUE)) source(f)
source("_Backtest/variants.R")
source("_Helper Files/Function Scripts/prior_functions.R")

cores <- if (.Platform$OS.type == "windows") 1 else max(1, parallel::detectCores() - 1)
first_prior <- c(MBB = 2021, WBB = 2023)   # first season the prior models can be tested on
cal_from    <- c(MBB = 2022, WBB = 2023)   # MBB 2021 (COVID season) is left out of the fit
out_dir <- "_Backtest/output"; pri_dir <- "_Backtest/priors/output/priors_prod"
dir.create(pri_dir, recursive = TRUE, showWarnings = FALSE)
empty_dir <- file.path(out_dir, "empty"); dir.create(empty_dir, showWarnings = FALSE)

cal_path <- list(MBB = "VRGL/Stats/Team and Player Stats/Power Ratings/Weights",
                 WBB = "BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights")
cal_file <- function(lg, no_prior = FALSE)
  file.path(cal_path[[lg]], sprintf("calibration_%s%s.rds", tolower(lg), if (no_prior) "_no_prior" else ""))
fs_dir <- function(lg) file.path(dirname(dirname(lg_paths(lg)$fs)), "Full Season - Current Weights")

backup <- file.path("_Offseason Update", "backup", format(Sys.Date()))
dir.create(backup, recursive = TRUE, showWarnings = FALSE)

for (lg in c("MBB", "WBB")) {
  t0 <- Sys.time()
  seasons <- first_prior[[lg]]:dante_completed
  models <- readRDS(file.path(cal_path[[lg]], sprintf("prior_models_%s.rds", tolower(lg))))

  ## a. Priors for each past season -----
  cat("\n=====", lg, ": priors for", min(seasons), "-", max(seasons), "=====\n")
  for (s in seasons) {
    pr <- suppressWarnings(build_priors(lg, s, models, fs_dir = fs_dir(lg)))
    saveRDS(pr, file.path(pri_dir, basename(fill_path(lg_paths(lg)$priors, s))))
    cat(s, "")
  }
  cat("\n")

  ## b. Walk-forward backtests -----
  cat("Backtest with priors ...\n")
  Sys.setenv(BT_PRIORS_DIR = pri_dir)
  p_prior <- run_model(variants$PROD, lg, seasons, priors = "prod", cores = cores)
  saveRDS(p_prior, file.path(out_dir, sprintf("pred_PROD_prior_%s.rds", lg)))
  if (RUN_NO_PRIOR) {
    cat("Backtest without priors ...\n")
    Sys.setenv(BT_PRIORS_DIR = empty_dir)
    p_none <- run_model(variants$PROD, lg, bt_seasons[[lg]], priors = "prod", cores = cores)
    saveRDS(p_none, file.path(out_dir, sprintf("pred_PROD_noprior_%s.rds", lg)))
  }
  Sys.unsetenv("BT_PRIORS_DIR")

  ## c. Fit and save -----
  file.copy(cal_file(lg), backup, overwrite = TRUE)
  h <- add_scores(p_prior) %>% filter(season >= cal_from[[lg]])
  cal <- fit_calibration(h, by_phase = TRUE, hfa = TRUE)
  cal$league <- lg; cal$fitted_on_seasons <- sort(unique(h$season)); cal$built <- as.character(Sys.Date())
  cal$source <- "PROD walk-forward predictions with rebuilt priors (_Offseason Update/7. Rebuild Calibration.R)"
  saveRDS(cal, cal_file(lg))
  cat("\n--", lg, "calibration, seasons", paste(range(h$season), collapse = "-"), "| home-court", round(cal$hfa, 2), "\n")
  print(cal$phase_table)

  if (RUN_NO_PRIOR) {
    file.copy(cal_file(lg, TRUE), backup, overwrite = TRUE)
    hn <- add_scores(p_none)
    caln <- fit_calibration(hn, by_phase = TRUE, hfa = TRUE)
    caln$league <- lg; caln$fitted_on_seasons <- sort(unique(hn$season)); caln$built <- as.character(Sys.Date())
    caln$source <- "PROD walk-forward predictions, no priors (_Offseason Update/7. Rebuild Calibration.R)"
    saveRDS(caln, cal_file(lg, TRUE))
    cat("-- no-prior calibration, seasons", paste(range(hn$season), collapse = "-"), "\n")
    print(caln$phase_table)
  }

  ## Check: how the calibrated backtest did, season by season -----
  chk <- calibrate_layers(add_scores(p_prior) %>% filter(season >= cal_from[[lg]] - 1))
  cat("\n-- walk-forward check (each season calibrated on earlier seasons only) --\n")
  print(summarise_metrics(chk, season) %>% select(season, n, margin_rmse, logloss, total_rmse, total_bias) %>%
          as.data.frame(), row.names = FALSE)
  cat(lg, "done in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")
}
cat("\nPrevious calibration files are in", backup, "\n")
