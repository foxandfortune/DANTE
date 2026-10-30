# DANTE season setting ---------------------------------------------------------------
# The ONE place the year lives. Every daily, tournament, offseason and backtest script
# reads it: the daily scripts with source("_Helper Files/season.R") (they run from
# the repo root), the offseason scripts through their dante_path() helper.
#
# Spring-year convention: the 2026-27 season is 2027.
# Change it once a year, after both NCAA tournaments are over: set it to the coming
# season. From then on every offseason script works on the right years:
#   dante_season     the coming season (priors, shot models, conferences, daily sims)
#   dante_completed  the season that just finished (full-season ratings, recruiting
#                    class and departures, shot file, backtest/calibration through here)

dante_season    <- 2027
dante_completed <- dante_season - 1
