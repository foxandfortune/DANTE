# DANTE backtest harness: configuration ---------------------------------------
# All paths are relative to the DANTE repo root (the working directory when the
# harness runs, same convention as the production scripts).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(glmnet)
  library(Matrix)
})

`%!in%` <- Negate(`%in%`)

bt_paths <- list(
  MBB = list(
    poss     = "VRGL/Stats/Team and Player Stats/Power Ratings/Raw Data/poss_stats_with_types_{season}.rds",
    teams    = "_Helper Files/Team Data/team_database.rds",
    priors   = "VRGL/Stats/Team and Player Stats/Power Ratings/Team Ratings/Priors/priors_{season}.rds",
    w_single = "VRGL/Stats/Team and Player Stats/Power Ratings/Weights/single_season_{stat}_wgt.rds",
    w_prior  = "VRGL/Stats/Team and Player Stats/Power Ratings/Weights/with_priors_{stat}_wgt.rds",
    inseason = "VRGL/Stats/Team and Player Stats/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_{season}.rds",
    results  = "VRGL/Stats/Season Schedules/Results/{season}",
    disp_rtg = 13      # regular-season value actually used in 2026 (see git history)
  ),
  WBB = list(
    poss     = "BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Raw Data/poss_stats_with_types_wbb_{season}.rds",
    teams    = "_Helper Files/Team Data/team_database_wbb.rds",
    priors   = "BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Priors/priors_wbb_{season}.rds",
    w_single = "BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights/single_season_wbb_{stat}_wgt.rds",
    # Production BTRC loads the men's prior-blending weights (no WBB version exists)
    w_prior  = "VRGL/Stats/Team and Player Stats/Power Ratings/Weights/with_priors_{stat}_wgt.rds",
    inseason = "BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_wbb_{season}.rds",
    results  = "BTRC/Stats/Season Schedules/Results/{season}",
    disp_rtg = 13
  )
)

bt_path <- function(league, key, ...) {
  vals <- list(...)
  as.character(glue::glue_data(vals, bt_paths[[league]][[key]]))
}

# Seasons available for walk-forward testing (first season of data is used only
# as the source of the "simple" prior for the next season)
# Through the last finished season, from _Helper Files/season.R
source("_Helper Files/season.R")
bt_seasons <- list(MBB = 2019:dante_completed, WBB = 2022:dante_completed)

# Production component / rating specs ------------------------------------------
prod_stats <- c(pace = "poss_per_40", ast = "ast_rt", oreb = "oreb_rt",
                to = "to_rt", rtg = "rtg")

prod_features <- list(
  poss_per_40 = c("game_date", "team_id", "opp_id"),
  ast_rt      = c("game_date", "team_id", "opp_id"),
  oreb_rt     = c("game_date", "team_id", "opp_id"),
  to_rt       = c("game_date", "team_id", "opp_id"),
  rtg         = c("game_date", "team_id", "opp_id",
                  "poss_per_40", "ast_rt", "oreb_rt", "to_rt",
                  "is_home", "neutral_site", "days_rest", "travel")
)
