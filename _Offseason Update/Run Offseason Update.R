# ──────────────────────────────────────────────────────────────────────────────
# Run Offseason Update.R
#
# Runs the scripted part of the offseason (steps 3-8 in README.md) in order, each in
# its own R session, logging to _Offseason Update/logs/. Stops at the first step that
# fails and shows the end of its log.
#
# Before running:
#   0. Set dante_season in _Helper Files/season.R to the coming season.
#   1-2. Team data, conferences and the men's recruiting scrapes (interactive; see
#        README.md). The check in step 8 tells you if anything is missing.
#
# Pick what to run below. A full run takes roughly 2-3 hours on Windows; most of it is
# step 4 (downloading four seasons of play-by-play) and step 7 (backtests).
# ──────────────────────────────────────────────────────────────────────────────
run_steps <- c(3, 4, 5, 6, 7, 8)

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
rscript <- file.path(R.home("bin"), "Rscript")
log_dir <- file.path("_Offseason Update", "logs", format(Sys.time(), "%Y-%m-%d_%H%M"))
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

steps <- list(
  `3` = c("Full-season ratings, men's"   = "VRGL/Stats/Offseason Updates - MBB/Full Season Ratings/Full-Season Ratings - All.R",
          "Full-season ratings, women's" = "BTRC/Stats/Offseason Updates - WBB/Full Season Ratings/Full-Season Ratings WBB - All.R"),
  `4` = c("Shot file, men's"             = "VRGL/Stats/Offseason Updates - MBB/Shooting Models/_Create Shot File for Models - MBB.R",
          "Shot models, men's"           = "VRGL/Stats/Offseason Updates - MBB/Shooting Models/_Model Maker - All Models (Preseason) - MBB.R",
          "Shot file, women's"           = "BTRC/Stats/Offseason Updates - WBB/Shooting Models/_Create Shot File for Models - WBB.R",
          "Shot models, women's"         = "BTRC/Stats/Offseason Updates - WBB/Shooting Models/_Model Maker - All Models (Preseason) - WBB.R"),
  `5` = c("Prior models"                 = "_Offseason Update/5. Refresh Prior Models.R"),
  `6` = c("Priors, men's"                = "VRGL/Stats/Offseason Updates - MBB/Recruiting and Priors/4. Create Priors - MBB.R",
          "Priors, women's"              = "BTRC/Stats/Offseason Updates - WBB/Recruiting and Priors/_Create Priors (Simple) - WBB.R"),
  `7` = c("Calibration"                  = "_Offseason Update/7. Rebuild Calibration.R"),
  `8` = c("Season check"                 = "_Offseason Update/8. Check Season Ready.R"))

cat("Offseason update for the", dante_season, "season (finished season:", dante_completed, ")\n")
for (s in as.character(run_steps)) {
  for (nm in names(steps[[s]])) {
    script <- steps[[s]][[nm]]
    log <- file.path(log_dir, sprintf("%s - %s.log", s, nm))
    t0 <- Sys.time()
    cat(sprintf("\n[%s] Step %s: %s ...", format(t0, "%H:%M"), s, nm))
    res <- suppressWarnings(system2(rscript, shQuote(script), stdout = TRUE, stderr = TRUE))
    writeLines(res, log)
    status <- if (is.null(attr(res, "status"))) 0 else attr(res, "status")
    cat(sprintf(" %s (%.1f min)\n", if (status == 0) "done" else "FAILED",
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    if (status != 0) {
      cat("\n---- last lines of", log, "----\n"); cat(tail(readLines(log, warn = FALSE), 30), sep = "\n")
      stop("Step ", s, " (", nm, ") failed. Fix it, then rerun from this step (set run_steps).")
    }
  }
}
cat("\nAll steps finished. Logs:", log_dir, "\n")
cat("Last: commit and push. Re-enable the daily schedule in .github/workflows/vrgl-pipeline.yml once games start.\n")
