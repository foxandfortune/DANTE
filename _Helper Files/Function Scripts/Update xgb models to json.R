library(xgboost)

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
source(dante_path("_Helper Files", "season.R"))
cur_yr <- dante_season

# MBB ------------------------
# Load and re-save the location model
xfg_loc <- xgb.load(glue::glue("VRGL/Stats/Offseason Updates - MBB/Shooting Models/Models/xfg {cur_yr}.model"))
xgb.save(xfg_loc, glue::glue("VRGL/Stats/Offseason Updates - MBB/Shooting Models/Models/xfg {cur_yr}.json"))

# Load and re-save the no-location model
xfg_noloc <- xgb.load(glue::glue("VRGL/Stats/Offseason Updates - MBB/Shooting Models/Models/xfg noloc {cur_yr}.model"))
xgb.save(xfg_noloc, glue::glue("VRGL/Stats/Offseason Updates - MBB/Shooting Models/Models/xfg noloc {cur_yr}.json"))

# WBB ----------------------------------------
# Load and re-save the location model
xfg_loc <- xgb.load(glue::glue("BTRC/Stats/Offseason Updates - WBB/Shooting Models/Models/xfg wbb {cur_yr}.model"))
xgb.save(xfg_loc, glue::glue("BTRC/Stats/Offseason Updates - WBB/Shooting Models/Models/xfg wbb {cur_yr}.json"))

# Load and re-save the no-location model
xfg_noloc <- xgb.load(glue::glue("BTRC/Stats/Offseason Updates - WBB/Shooting Models/Models/xfg noloc wbb {cur_yr}.model"))
xgb.save(xfg_noloc, glue::glue("BTRC/Stats/Offseason Updates - WBB/Shooting Models/Models/xfg noloc wbb {cur_yr}.json"))
