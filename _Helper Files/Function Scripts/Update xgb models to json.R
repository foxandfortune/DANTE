library(xgboost)

cur_yr <- 2026

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
