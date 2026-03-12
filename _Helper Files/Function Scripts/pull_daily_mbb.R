# ─────────────────────────────────────────────────────────────────────────────
# pull_daily_mbb.R
#
# Functions to pull MBB (Men's Basketball) data directly from ESPN's API
# for specific dates. Use these when hoopR's cached parquet files are not
# up to date (typically within the last ~24-48 hours).
#
# Requires: hoopR, tidyverse, purrr
#
# USAGE EXAMPLES:
#   source("_Helper Files/Function Scripts/pull_daily_mbb.R")
#
#   # Single day:
#   result <- pull_mbb_daily_data("2026-03-10")
#
#   # Date range (catch-up):
#   result <- pull_mbb_daily_data("2026-03-07", "2026-03-10")
#
#   # Access each data type:
#   pbp      <- result$pbp
#   team_box <- result$team_box
#   player_box <- result$player_box
#   schedule <- result$schedule
#
#   # Or pull individually:
#   pbp      <- pull_mbb_pbp_by_date("2026-03-10")
#   team_box <- pull_mbb_team_box_by_date("2026-03-10")
#   player_box <- pull_mbb_player_box_by_date("2026-03-10")
#   schedule <- pull_mbb_schedule_by_date("2026-03-10")
# ─────────────────────────────────────────────────────────────────────────────

library(hoopR)
library(tidyverse)
library(purrr)


# ─────────────────────────────────────────────────────────────────────────────
# Internal: get completed game IDs for a single date
# ─────────────────────────────────────────────────────────────────────────────
.get_mbb_game_ids_for_date <- function(date) {
  d_str <- format(as.Date(date), "%Y%m%d")
  tryCatch({
    sb <- hoopR::espn_mbb_scoreboard(date = d_str)
    if (is.null(sb) || nrow(sb) == 0) {
      message(sprintf("  [MBB] No games found for %s", date))
      return(character(0))
    }
    # Keep only completed games
    completed <- sb %>%
      filter(status_type_completed == TRUE | status_type_name == "STATUS_FINAL")
    ids <- as.character(completed$id)
    message(sprintf("  [MBB] %s: found %d completed game(s)", date, length(ids)))
    return(ids)
  }, error = function(e) {
    message(sprintf("  [MBB] Could not fetch scoreboard for %s: %s", date, e$message))
    return(character(0))
  })
}


# ─────────────────────────────────────────────────────────────────────────────
# Internal: normalise a date range into a vector of Date objects
# ─────────────────────────────────────────────────────────────────────────────
.date_seq <- function(date_start, date_end = NULL) {
  if (is.null(date_end)) date_end <- date_start
  seq(as.Date(date_start), as.Date(date_end), by = "day")
}


# ─────────────────────────────────────────────────────────────────────────────
# pull_mbb_schedule_by_date()
#
# Returns a data frame of schedule/scoreboard rows for the given date(s),
# formatted to match hoopR::load_mbb_schedule() as closely as possible.
# ─────────────────────────────────────────────────────────────────────────────
pull_mbb_schedule_by_date <- function(date_start, date_end = NULL) {
  dates <- .date_seq(date_start, date_end)

  all_rows <- map_df(dates, function(d) {
    d_str <- format(d, "%Y%m%d")
    tryCatch({
      sb <- hoopR::espn_mbb_scoreboard(date = d_str)
      if (is.null(sb) || nrow(sb) == 0) return(NULL)
      as.data.frame(sb)
    }, error = function(e) {
      message(sprintf("  [MBB schedule] Error on %s: %s", d, e$message))
      NULL
    })
  })

  if (is.null(all_rows) || nrow(all_rows) == 0) {
    message("  [MBB schedule] No schedule data returned.")
    return(data.frame())
  }

  # Rename to match load_mbb_schedule() conventions where possible
  # The scoreboard `id` column is the game_id used everywhere else
  out <- all_rows %>%
    rename_with(~ case_when(
      .x == "id"                        ~ "game_id",
      .x == "date"                      ~ "game_date_time",
      .x == "home_team_id"              ~ "home_id",
      .x == "away_team_id"              ~ "away_id",
      .x == "home_team_score"           ~ "home_score",
      .x == "away_team_score"           ~ "away_score",
      .x == "venue_id"                  ~ "venue_id",
      TRUE                              ~ .x
    ), .cols = everything()) %>%
    mutate(
      game_id   = as.character(game_id),
      game_date = as.Date(game_date_time)
    )

  return(out)
}


# ─────────────────────────────────────────────────────────────────────────────
# pull_mbb_pbp_by_date()
#
# Returns play-by-play data for all completed MBB games on the given date(s),
# matching the schema returned by hoopR::load_mbb_pbp().
# ─────────────────────────────────────────────────────────────────────────────
pull_mbb_pbp_by_date <- function(date_start, date_end = NULL, sleep_secs = 0.5) {
  dates  <- .date_seq(date_start, date_end)

  # Collect all game IDs across dates
  all_ids <- unlist(map(dates, .get_mbb_game_ids_for_date))
  all_ids <- unique(all_ids[!is.na(all_ids) & nchar(all_ids) > 0])

  if (length(all_ids) == 0) {
    message("  [MBB PBP] No game IDs found for specified date(s).")
    return(data.frame())
  }

  message(sprintf("  [MBB PBP] Pulling PBP for %d game(s)...", length(all_ids)))

  pbp_list <- map(all_ids, function(gid) {
    Sys.sleep(sleep_secs)
    tryCatch({
      result <- hoopR::espn_mbb_pbp(game_id = gid)
      if (!is.null(result) && nrow(result) > 0) {
        as.data.frame(result)
      } else {
        NULL
      }
    }, error = function(e) {
      message(sprintf("    [MBB PBP] game_id %s failed: %s", gid, e$message))
      NULL
    })
  })

  pbp_list <- pbp_list[!sapply(pbp_list, is.null)]

  if (length(pbp_list) == 0) {
    message("  [MBB PBP] No PBP data returned for any game.")
    return(data.frame())
  }

  out <- bind_rows(pbp_list)
  message(sprintf("  [MBB PBP] Done. %d rows across %d games.", nrow(out), length(pbp_list)))
  return(out)
}


# ─────────────────────────────────────────────────────────────────────────────
# pull_mbb_player_box_by_date()
#
# Returns player box score / roster data for all completed MBB games on the
# given date(s), matching the schema returned by hoopR::load_mbb_player_box().
# ─────────────────────────────────────────────────────────────────────────────
pull_mbb_player_box_by_date <- function(date_start, date_end = NULL, sleep_secs = 0.5) {
  dates  <- .date_seq(date_start, date_end)
  all_ids <- unlist(map(dates, .get_mbb_game_ids_for_date))
  all_ids <- unique(all_ids[!is.na(all_ids) & nchar(all_ids) > 0])

  if (length(all_ids) == 0) {
    message("  [MBB player box] No game IDs found for specified date(s).")
    return(data.frame())
  }

  message(sprintf("  [MBB player box] Pulling player box for %d game(s)...", length(all_ids)))

  box_list <- map(all_ids, function(gid) {
    Sys.sleep(sleep_secs)
    tryCatch({
      result <- hoopR::espn_mbb_player_box(game_id = gid)
      if (!is.null(result) && nrow(result) > 0) as.data.frame(result) else NULL
    }, error = function(e) {
      message(sprintf("    [MBB player box] game_id %s failed: %s", gid, e$message))
      NULL
    })
  })

  box_list <- box_list[!sapply(box_list, is.null)]

  if (length(box_list) == 0) {
    message("  [MBB player box] No player box data returned.")
    return(data.frame())
  }

  out <- bind_rows(box_list)
  message(sprintf("  [MBB player box] Done. %d rows across %d games.", nrow(out), length(box_list)))
  return(out)
}


# ─────────────────────────────────────────────────────────────────────────────
# pull_mbb_team_box_by_date()
#
# Returns team box score data for all completed MBB games on the given date(s),
# matching the schema returned by hoopR::load_mbb_team_box().
# ─────────────────────────────────────────────────────────────────────────────
pull_mbb_team_box_by_date <- function(date_start, date_end = NULL, sleep_secs = 0.5) {
  dates  <- .date_seq(date_start, date_end)
  all_ids <- unlist(map(dates, .get_mbb_game_ids_for_date))
  all_ids <- unique(all_ids[!is.na(all_ids) & nchar(all_ids) > 0])

  if (length(all_ids) == 0) {
    message("  [MBB team box] No game IDs found for specified date(s).")
    return(data.frame())
  }

  message(sprintf("  [MBB team box] Pulling team box for %d game(s)...", length(all_ids)))

  box_list <- map(all_ids, function(gid) {
    Sys.sleep(sleep_secs)
    tryCatch({
      result <- hoopR::espn_mbb_team_box(game_id = gid)
      if (!is.null(result) && nrow(result) > 0) as.data.frame(result) else NULL
    }, error = function(e) {
      message(sprintf("    [MBB team box] game_id %s failed: %s", gid, e$message))
      NULL
    })
  })

  box_list <- box_list[!sapply(box_list, is.null)]

  if (length(box_list) == 0) {
    message("  [MBB team box] No team box data returned.")
    return(data.frame())
  }

  out <- bind_rows(box_list)
  message(sprintf("  [MBB team box] Done. %d rows across %d games.", nrow(out), length(box_list)))
  return(out)
}


# ─────────────────────────────────────────────────────────────────────────────
# pull_mbb_daily_data()
#
# Master convenience function. Pulls PBP, player box, team box, and schedule
# for all completed MBB games across the given date range in one call.
#
# Returns a named list:
#   $pbp        – play-by-play (matches load_mbb_pbp() schema)
#   $player_box – player box scores (matches load_mbb_player_box() schema)
#   $team_box   – team box scores (matches load_mbb_team_box() schema)
#   $schedule   – schedule / scoreboard rows
# ─────────────────────────────────────────────────────────────────────────────
pull_mbb_daily_data <- function(date_start, date_end = NULL, sleep_secs = 0.5) {
  dates   <- .date_seq(date_start, date_end)
  date_label <- if (length(dates) == 1) {
    as.character(dates[1])
  } else {
    paste0(dates[1], " to ", dates[length(dates)])
  }

  message(sprintf("\n===== MBB Daily Pull: %s =====", date_label))

  # Collect game IDs once so we don't hit the scoreboard endpoint 4x
  all_ids <- unlist(map(dates, .get_mbb_game_ids_for_date))
  all_ids <- unique(all_ids[!is.na(all_ids) & nchar(all_ids) > 0])

  if (length(all_ids) == 0) {
    message("  [MBB] No completed games found. Returning empty list.")
    return(list(pbp = data.frame(), player_box = data.frame(),
                team_box = data.frame(), schedule = data.frame()))
  }

  message(sprintf("  [MBB] Total: %d unique game(s) to pull.", length(all_ids)))

  # ── PBP ──────────────────────────────────────────────────────────────────
  message("\n  >> Pulling PBP...")
  pbp_list <- map(all_ids, function(gid) {
    Sys.sleep(sleep_secs)
    tryCatch({
      r <- hoopR::espn_mbb_pbp(game_id = gid)
      if (!is.null(r) && nrow(r) > 0) as.data.frame(r) else NULL
    }, error = function(e) {
      message(sprintf("     PBP game %s failed: %s", gid, e$message)); NULL
    })
  })
  pbp <- bind_rows(pbp_list[!sapply(pbp_list, is.null)])
  message(sprintf("     PBP: %d rows", nrow(pbp)))

  # ── Player box ────────────────────────────────────────────────────────────
  message("  >> Pulling player box scores...")
  pbox_list <- map(all_ids, function(gid) {
    Sys.sleep(sleep_secs)
    tryCatch({
      r <- hoopR::espn_mbb_player_box(game_id = gid)
      if (!is.null(r) && nrow(r) > 0) as.data.frame(r) else NULL
    }, error = function(e) {
      message(sprintf("     Player box game %s failed: %s", gid, e$message)); NULL
    })
  })
  player_box <- bind_rows(pbox_list[!sapply(pbox_list, is.null)])
  message(sprintf("     Player box: %d rows", nrow(player_box)))

  # ── Team box ─────────────────────────────────────────────────────────────
  message("  >> Pulling team box scores...")
  tbox_list <- map(all_ids, function(gid) {
    Sys.sleep(sleep_secs)
    tryCatch({
      r <- hoopR::espn_mbb_team_box(game_id = gid)
      if (!is.null(r) && nrow(r) > 0) as.data.frame(r) else NULL
    }, error = function(e) {
      message(sprintf("     Team box game %s failed: %s", gid, e$message)); NULL
    })
  })
  team_box <- bind_rows(tbox_list[!sapply(tbox_list, is.null)])
  message(sprintf("     Team box: %d rows", nrow(team_box)))

  # ── Schedule ─────────────────────────────────────────────────────────────
  message("  >> Pulling schedule / scoreboard rows...")
  sched_list <- map(dates, function(d) {
    d_str <- format(d, "%Y%m%d")
    tryCatch({
      r <- hoopR::espn_mbb_scoreboard(date = d_str)
      if (!is.null(r) && nrow(r) > 0) as.data.frame(r) else NULL
    }, error = function(e) {
      message(sprintf("     Schedule %s failed: %s", d, e$message)); NULL
    })
  })
  schedule <- bind_rows(sched_list[!sapply(sched_list, is.null)])
  if (nrow(schedule) > 0 && "id" %in% names(schedule)) {
    schedule <- schedule %>% rename(game_id = id)
  }
  message(sprintf("     Schedule: %d rows", nrow(schedule)))

  message(sprintf("\n===== MBB Daily Pull complete =====\n"))

  return(list(
    pbp        = pbp,
    player_box = player_box,
    team_box   = team_box,
    schedule   = schedule
  ))
}
