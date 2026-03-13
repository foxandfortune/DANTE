# =============================================================================
# MBB Backup Data Pull — ESPN API Direct
# Bypasses hoopR's stale pre-built parquet files and hits ESPN API live.
#
# Replaces:
#   hoopR::load_mbb_pbp(seasons = cur_yr)
#   hoopR::load_mbb_schedule(seasons = cur_yr)
#   hoopR::load_mbb_team_box(cur_yr)
#
# How hoopR's load_mbb_* works:
#   It downloads pre-built parquet files from GitHub Releases on sportsdataverse-data.
#   Those files only update when the maintainer runs their scrape pipeline.
#   If that pipeline stalls (as of ~March 6), data stops refreshing.
#
# This script uses hoopR's LIVE ESPN API functions instead, which hit ESPN directly.
# =============================================================================

library(hoopR)
library(dplyr)
library(purrr)

cur_yr <- 2026

# =============================================================================
# STEP 1 — SCHEDULE
# Replaces: hoopR::load_mbb_schedule(seasons = cur_yr)
#
# ESPN scoreboard API can be queried by date or by conference.
# espn_mbb_scoreboard() returns games for a given date.
# We'll pull a date range covering the current tournament window.
# =============================================================================

#' Pull MBB schedule/scoreboard for a range of dates via ESPN API
#'
#' @param start_date character "YYYYMMDD"
#' @param end_date   character "YYYYMMDD"
#' @return data.frame of games in that window
pull_mbb_schedule_live <- function(start_date = "20260307", end_date = format(Sys.Date()-1, "%Y%m%d")) {

  dates <- seq(as.Date(start_date, "%Y%m%d"),
               as.Date(end_date,   "%Y%m%d"),
               by = "day")
  date_strings <- format(dates, "%Y%m%d")

  message("Pulling ESPN scoreboard for ", length(date_strings), " dates...")

  results <- map_dfr(date_strings, function(d) {
    tryCatch({
      Sys.sleep(0.3) # be polite to ESPN API
      df <- hoopR::espn_mbb_scoreboard(season = d)
      if (!is.null(df) && nrow(df) > 0) df else NULL
    }, error = function(e) {
      message("  No data for ", d, ": ", conditionMessage(e))
      NULL
    })
  })

  if (nrow(results) == 0) {
    message("No schedule data returned for date range.")
    return(tibble())
  }

  message("Retrieved ", nrow(results), " games from ", min(results$game_date, na.rm=TRUE),
          " to ", max(results$game_date, na.rm=TRUE))
  results
}

# Pull schedule for games since last known good date
schedule_new <- pull_mbb_schedule_live(start_date = "20260307")

# Inspect what you got
glimpse(schedule_new)
cat("Game IDs available:\n")
print(sort(unique(schedule_new$game_id)))


# =============================================================================
# STEP 2 — PBP + TEAM BOX (per game)
# Replaces: hoopR::load_mbb_pbp(seasons = cur_yr)
#           hoopR::load_mbb_team_box(cur_yr)
#
# espn_mbb_game_all() returns a named list: $Plays, $Team, $Player
# We loop over the game IDs we found in the schedule.
# =============================================================================

#' Pull PBP and box scores for a vector of ESPN game IDs
#'
#' @param game_ids  integer or character vector of ESPN game IDs
#' @param delay_sec pause between requests (default 0.5s — be polite)
#' @return list with $pbp (all plays) and $team_box (all team box scores)
pull_mbb_game_data_live <- function(game_ids, delay_sec = 0.5) {

  n <- length(game_ids)
  message("Pulling data for ", n, " games...")

  pbp_list      <- vector("list", n)
  team_box_list <- vector("list", n)

  for (i in seq_along(game_ids)) {
    gid <- game_ids[i]
    if (i %% 10 == 0) message("  Progress: ", i, "/", n)
    Sys.sleep(delay_sec)

    tryCatch({
      result <- hoopR::espn_mbb_game_all(game_id = gid)

      if (!is.null(result$Plays) && nrow(result$Plays) > 0) {
        pbp_list[[i]] <- result$Plays %>% mutate(game_id = as.character(gid))
      }
      if (!is.null(result$Team) && nrow(result$Team) > 0) {
        team_box_list[[i]] <- result$Team %>% mutate(game_id = as.character(gid))
      }
    }, error = function(e) {
      message("  Error on game ", gid, ": ", conditionMessage(e))
    })
  }

  list(
    pbp      = bind_rows(pbp_list),
    team_box = bind_rows(team_box_list)
  )
}

# Get game IDs from your new schedule pull
new_game_ids <- unique(schedule_new$game_id)
cat("\nFound", length(new_game_ids), "new game IDs to pull\n")

# Pull PBP + team box for all new games
new_game_data <- pull_mbb_game_data_live(game_ids = new_game_ids)

pbp_new      <- new_game_data$pbp
team_box_new <- new_game_data$team_box

message("PBP rows: ",       nrow(pbp_new))
message("Team box rows: ",  nrow(team_box_new))


# =============================================================================
# STEP 3 — MERGE WITH YOUR EXISTING hoopR DATA
# Combine the stale hoopR data (through ~Mar 6) with the fresh ESPN API pull.
# =============================================================================

merge_with_existing <- function(existing_hoopR_data, new_data, id_col = "game_id") {
  if (is.null(existing_hoopR_data) || nrow(existing_hoopR_data) == 0) {
    return(new_data)
  }
  existing_ids <- unique(existing_hoopR_data[[id_col]])
  new_only     <- new_data %>% filter(!(.data[[id_col]] %in% existing_ids))
  message("  Adding ", nrow(new_only), " new rows (", length(unique(new_only[[id_col]])), " new games)")
  bind_rows(existing_hoopR_data, new_only)
}

# Load your stale hoopR baseline
message("\nLoading stale hoopR data as baseline...")
mbb_pbp_base      <- hoopR::load_mbb_pbp(seasons = cur_yr)
mbb_schedule_base <- hoopR::load_mbb_schedule(seasons = cur_yr)
mbb_team_box_base <- hoopR::load_mbb_team_box(cur_yr)

# Merge
message("\nMerging PBP...")
mbb_pbp_full <- merge_with_existing(mbb_pbp_base, pbp_new)

message("Merging schedule...")
mbb_schedule_full <- merge_with_existing(mbb_schedule_base, schedule_new)

message("Merging team box...")
mbb_team_box_full <- merge_with_existing(mbb_team_box_base, team_box_new)

# Final counts
message("\n=== FINAL ROW COUNTS ===")
message("PBP:      ", nrow(mbb_pbp_full),      " rows, ", length(unique(mbb_pbp_full$game_id)),      " games")
message("Schedule: ", nrow(mbb_schedule_full),  " rows, ", length(unique(mbb_schedule_full$game_id)), " games")
message("Team box: ", nrow(mbb_team_box_full),  " rows, ", length(unique(mbb_team_box_full$game_id)), " games")

# Check latest dates
message("\n=== LATEST DATES IN MERGED DATA ===")
if ("game_date" %in% names(mbb_schedule_full)) {
  message("Schedule latest date: ", max(mbb_schedule_full$game_date, na.rm = TRUE))
}
if ("game_date" %in% names(mbb_pbp_full)) {
  message("PBP latest date:      ", max(mbb_pbp_full$game_date, na.rm = TRUE))
}


# =============================================================================
# STEP 4 — SAVE (optional)
# Save to local parquet or RDS so you don't re-pull everything each session.
# =============================================================================

# Option A: RDS (no extra packages needed)
# saveRDS(mbb_pbp_full,      "data/mbb_pbp_2026.rds")
# saveRDS(mbb_schedule_full, "data/mbb_schedule_2026.rds")
# saveRDS(mbb_team_box_full, "data/mbb_team_box_2026.rds")

# Option B: Parquet (matches hoopR's native format, requires arrow package)
# library(arrow)
# write_parquet(mbb_pbp_full,      "data/mbb_pbp_2026.parquet")
# write_parquet(mbb_schedule_full, "data/mbb_schedule_2026.parquet")
# write_parquet(mbb_team_box_full, "data/mbb_team_box_2026.parquet")


# =============================================================================
# DIAGNOSTICS — run these to understand WHY hoopR is stale
# =============================================================================

# 1. Check what date the pre-built files were last updated
diag_check_hoopR_release_date <- function() {
  message("\n=== CHECKING sportsdataverse-data RELEASE DATE ===")
  # The URL pattern hoopR uses for MBB PBP parquet files:
  url <- "https://github.com/sportsdataverse/sportsdataverse-data/releases/latest"
  message("Check manually: ", url)
  message("Or run: browseURL('", url, "')")

  # Check the raw parquet URL directly
  pbp_url <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/releases/download/",
    "espn_mens_college_basketball_pbp/espn_mens_college_basketball_pbp_",
    cur_yr, ".parquet"
  )
  message("\nDirect parquet URL to test:\n", pbp_url)
  message("Try: httr::HEAD(pbp_url) to check last-modified header")
}

diag_check_hoopR_release_date()

# 2. Spot-check a specific known post-March-6 game directly from ESPN API
diag_spot_check_game <- function(game_id) {
  message("\n=== SPOT CHECK GAME ", game_id, " ===")
  tryCatch({
    result <- hoopR::espn_mbb_game_all(game_id = game_id)
    message("Plays:    ", nrow(result$Plays))
    message("Team box: ", nrow(result$Team))
    message("Players:  ", nrow(result$Player))
    print(head(result$Plays, 3))
  }, error = function(e) {
    message("Error: ", conditionMessage(e))
  })
}

# Example: find a game ID from your schedule pull and spot check it
if (length(new_game_ids) > 0) {
  diag_spot_check_game(new_game_ids[1])
}
