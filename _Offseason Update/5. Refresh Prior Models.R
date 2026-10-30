# ──────────────────────────────────────────────────────────────────────────────
# 5. Refresh Prior Models.R  (both leagues)
#
# Re-runs the prior model test (_Backtest/priors/_Test Priors - All Stats.R) with the
# season that just finished added, and saves the winning model for each stat x side
# as prior_models_mbb.rds / prior_models_wbb.rds in the Weights folders. Step 6
# (4. Create Priors / _Create Priors (Simple)) builds the new priors from these.
#
# Needs step 3 first: the finished season in "Full Season - Current Weights".
# The current model files are copied to _Offseason Update/backup/<date>/ before
# being replaced. Set SAVE <- FALSE to see the comparison without saving.
# Takes about 5-10 minutes.
# ──────────────────────────────────────────────────────────────────────────────
SAVE <- TRUE

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
suppressMessages({library(dplyr); library(tidyr)})

rscript <- file.path(R.home("bin"), "Rscript")
weights_dir <- c(MBB = "VRGL/Stats/Team and Player Stats/Power Ratings/Weights",
                 WBB = "BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights")
backup <- file.path("_Offseason Update", "backup", format(Sys.Date()))
dir.create(backup, recursive = TRUE, showWarnings = FALSE)

for (lg in c("MBB", "WBB")) {
  cat("\n=====", lg, ": testing prior models through", dante_completed, "=====\n")
  res_txt <- suppressWarnings(system2(rscript, c(shQuote("_Backtest/priors/_Test Priors - All Stats.R"), lg, "lm"),
                                      stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(res_txt, "status")) && attr(res_txt, "status") != 0) {
    cat(tail(res_txt, 30), sep = "\n"); stop("The prior test failed for ", lg)
  }
  i <- grep("CHOSEN MODEL", res_txt)
  if (length(i)) cat(res_txt[(i[1] - 1):min(length(res_txt), i[1] + 18)], sep = "\n")
  res <- readRDS(sprintf("_Backtest/priors/output/prior_model_tests_%s_lm.rds", lg))
  new <- as.data.frame(res$chosen[, c("stat", "side", "model", "label", "formula", "rmse", "rmse_current", "skill_vs_avg")])
  attr(new, "tested") <- list(date = Sys.Date(), test_seasons = res$config$TEST_SEASONS, engine = "lm",
                              parsimony_tol = res$config$PARSIMONY_TOL,
                              note = "Chosen by _Backtest/priors/_Test Priors - All Stats.R (_Offseason Update/5)")

  path <- file.path(weights_dir[[lg]], sprintf("prior_models_%s.rds", tolower(lg)))
  if (file.exists(path)) {
    old <- readRDS(path)
    cmp <- new %>% select(stat, side, new_model = model, new_rmse = rmse) %>%
      left_join(old %>% select(stat, side, old_model = model), by = c("stat", "side")) %>%
      mutate(changed = if_else(new_model != old_model, "<- changed", ""))
    cat("\nChosen models (rmse = prior error on held-out seasons):\n")
    print(cmp, row.names = FALSE)
  }
  if (SAVE) {
    if (file.exists(path)) file.copy(path, backup, overwrite = TRUE)
    saveRDS(new, path)
    cat("Saved", path, "\n")
  }
}
