# ─────────────────────────────────────────────────────────────────────────────
# run_daily_pull.R
#
# Master orchestration script for pulling fresh ESPN data when hoopR /
# wehoop cached parquet files are out of date.
#
# This script:
#   1. Pulls PBP, player box, team box, and schedule for both MBB and WBB
#   2. Appends only NEW game_ids to the existing season-long .rds files
#   3. Saves updated files back to their original locations
#
# HOW TO USE
# ──────────────────────────────────────────────────────────────────────────
# A) ONE-TIME CATCH-UP (games missed since last good package update):
#
#      run_mode  <- "catchup"
#      date_from <- "2026-03-07"   # first date to catch up from
#      date_to   <- Sys.Date() - 1 # yesterday (last full day of games)
#
# B) DAILY (run each morning for previous day's games):
#
#      run_mode  <- "daily"
#      # date_from / date_to are ignored; yesterday is used automatically
#
# Set the two variables below, then source / Run All.
# ─────────────────────────────────────────────────────────────────────────────

# ── CONFIGURE HERE ────────────────────────────────────────────────────────────
run_mode  <- "catchup"          # "catchup" or "daily"
date_from <- "2026-03-07"       # only used in "catchup" mode
date_to   <- Sys.Date() - 1    # only used in "catchup" mode
cur_yr    <- 2026               # current season year
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(hoopR)
library(wehoop)
library(purrr)

source("_Helper Files/Function Scripts/pull_daily_mbb.R")
source("_Helper Files/Function Scripts/pull_daily_wbb.R")

`%!in%` <- Negate(`%in%`)

# Resolve date range
if (run_mode == "daily") {
  pull_start <- as.character(Sys.Date() - 1)
  pull_end   <- as.character(Sys.Date() - 1)
  cat(sprintf("\nMode: DAILY — pulling games for %s\n", pull_start))
} else {
  pull_start <- as.character(date_from)
  pull_end   <- as.character(date_to)
  cat(sprintf("\nMode: CATCH-UP — pulling games from %s to %s\n", pull_start, pull_end))
}


# ═════════════════════════════════════════════════════════════════════════════
# MBB (VRGL)
# ═════════════════════════════════════════════════════════════════════════════
cat("\n─── MBB (VRGL) ───────────────────────────────────────────────────────────\n")

mbb <- pull_mbb_daily_data(date_start = pull_start, date_end = pull_end)

# ── PBP ──────────────────────────────────────────────────────────────────────
if (nrow(mbb$pbp) > 0) {
  pbp_path <- glue::glue("VRGL/Stats/Team and Player Stats/PBP and Shot Data/pbp_raw_daily_{cur_yr}.rds")

  # Load existing daily cache if it exists, otherwise start fresh
  if (file.exists(pbp_path)) {
    existing_pbp <- readRDS(pbp_path)
    new_pbp <- mbb$pbp %>%
      filter(game_id %!in% existing_pbp$game_id)
    updated_pbp <- bind_rows(existing_pbp, new_pbp)
    cat(sprintf("  MBB PBP: appended %d new rows (%d games added)\n",
                nrow(new_pbp), n_distinct(new_pbp$game_id)))
  } else {
    updated_pbp <- mbb$pbp
    cat(sprintf("  MBB PBP: created new cache with %d rows\n", nrow(updated_pbp)))
  }

  saveRDS(updated_pbp, pbp_path)
  Sys.setFileTime(pbp_path, Sys.time())
} else {
  cat("  MBB PBP: no new data to save.\n")
}

# ── Player box (roster) ───────────────────────────────────────────────────────
if (nrow(mbb$player_box) > 0) {
  pbox_path <- glue::glue("VRGL/Stats/Team and Player Stats/Rosters/roster_daily_{cur_yr}.rds")

  if (file.exists(pbox_path)) {
    existing_pbox <- readRDS(pbox_path)
    new_pbox <- mbb$player_box %>%
      filter(game_id %!in% existing_pbox$game_id)
    updated_pbox <- bind_rows(existing_pbox, new_pbox)
    cat(sprintf("  MBB player box: appended %d new rows\n", nrow(new_pbox)))
  } else {
    updated_pbox <- mbb$player_box
    cat(sprintf("  MBB player box: created new cache with %d rows\n", nrow(updated_pbox)))
  }

  saveRDS(updated_pbox, pbox_path)
  Sys.setFileTime(pbox_path, Sys.time())
} else {
  cat("  MBB player box: no new data to save.\n")
}

# ── Team box ──────────────────────────────────────────────────────────────────
if (nrow(mbb$team_box) > 0) {
  tbox_path <- glue::glue("VRGL/Stats/Team and Player Stats/Power Ratings/Raw Data/team_box_daily_{cur_yr}.rds")

  if (file.exists(tbox_path)) {
    existing_tbox <- readRDS(tbox_path)
    new_tbox <- mbb$team_box %>%
      filter(game_id %!in% existing_tbox$game_id)
    updated_tbox <- bind_rows(existing_tbox, new_tbox)
    cat(sprintf("  MBB team box: appended %d new rows\n", nrow(new_tbox)))
  } else {
    updated_tbox <- mbb$team_box
    cat(sprintf("  MBB team box: created new cache with %d rows\n", nrow(updated_tbox)))
  }

  saveRDS(updated_tbox, tbox_path)
  Sys.setFileTime(tbox_path, Sys.time())
} else {
  cat("  MBB team box: no new data to save.\n")
}

# ── Schedule ──────────────────────────────────────────────────────────────────
if (nrow(mbb$schedule) > 0) {
  sched_path <- glue::glue("VRGL/Stats/Team and Player Stats/Power Ratings/Raw Data/schedule_daily_{cur_yr}.rds")

  if (file.exists(sched_path)) {
    existing_sched <- readRDS(sched_path)
    new_sched <- mbb$schedule %>%
      filter(game_id %!in% existing_sched$game_id)
    updated_sched <- bind_rows(existing_sched, new_sched)
    cat(sprintf("  MBB schedule: appended %d new rows\n", nrow(new_sched)))
  } else {
    updated_sched <- mbb$schedule
    cat(sprintf("  MBB schedule: created new cache with %d rows\n", nrow(updated_sched)))
  }

  saveRDS(updated_sched, sched_path)
  Sys.setFileTime(sched_path, Sys.time())
} else {
  cat("  MBB schedule: no new data to save.\n")
}


# ═════════════════════════════════════════════════════════════════════════════
# WBB (BTRC)
# ═════════════════════════════════════════════════════════════════════════════
cat("\n─── WBB (BTRC) ───────────────────────────────────────────────────────────\n")

wbb <- pull_wbb_daily_data(date_start = pull_start, date_end = pull_end)

# ── PBP ──────────────────────────────────────────────────────────────────────
if (nrow(wbb$pbp) > 0) {
  pbp_path_wbb <- glue::glue("BTRC/Stats/Team and Player Stats - WBB/PBP and Shot Data/pbp_raw_daily_wbb_{cur_yr}.rds")

  if (file.exists(pbp_path_wbb)) {
    existing_pbp_wbb <- readRDS(pbp_path_wbb)
    new_pbp_wbb <- wbb$pbp %>%
      filter(game_id %!in% existing_pbp_wbb$game_id)
    updated_pbp_wbb <- bind_rows(existing_pbp_wbb, new_pbp_wbb)
    cat(sprintf("  WBB PBP: appended %d new rows (%d games added)\n",
                nrow(new_pbp_wbb), n_distinct(new_pbp_wbb$game_id)))
  } else {
    updated_pbp_wbb <- wbb$pbp
    cat(sprintf("  WBB PBP: created new cache with %d rows\n", nrow(updated_pbp_wbb)))
  }

  saveRDS(updated_pbp_wbb, pbp_path_wbb)
  Sys.setFileTime(pbp_path_wbb, Sys.time())
} else {
  cat("  WBB PBP: no new data to save.\n")
}

# ── Player box (roster) ───────────────────────────────────────────────────────
if (nrow(wbb$player_box) > 0) {
  pbox_path_wbb <- glue::glue("BTRC/Stats/Team and Player Stats - WBB/Rosters/roster_daily_wbb_{cur_yr}.rds")

  if (file.exists(pbox_path_wbb)) {
    existing_pbox_wbb <- readRDS(pbox_path_wbb)
    new_pbox_wbb <- wbb$player_box %>%
      filter(game_id %!in% existing_pbox_wbb$game_id)
    updated_pbox_wbb <- bind_rows(existing_pbox_wbb, new_pbox_wbb)
    cat(sprintf("  WBB player box: appended %d new rows\n", nrow(new_pbox_wbb)))
  } else {
    updated_pbox_wbb <- wbb$player_box
    cat(sprintf("  WBB player box: created new cache with %d rows\n", nrow(updated_pbox_wbb)))
  }

  saveRDS(updated_pbox_wbb, pbox_path_wbb)
  Sys.setFileTime(pbox_path_wbb, Sys.time())
} else {
  cat("  WBB player box: no new data to save.\n")
}

# ── Team box ──────────────────────────────────────────────────────────────────
if (nrow(wbb$team_box) > 0) {
  tbox_path_wbb <- glue::glue("BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Raw Data/team_box_daily_wbb_{cur_yr}.rds")

  if (file.exists(tbox_path_wbb)) {
    existing_tbox_wbb <- readRDS(tbox_path_wbb)
    new_tbox_wbb <- wbb$team_box %>%
      filter(game_id %!in% existing_tbox_wbb$game_id)
    updated_tbox_wbb <- bind_rows(existing_tbox_wbb, new_tbox_wbb)
    cat(sprintf("  WBB team box: appended %d new rows\n", nrow(new_tbox_wbb)))
  } else {
    updated_tbox_wbb <- wbb$team_box
    cat(sprintf("  WBB team box: created new cache with %d rows\n", nrow(updated_tbox_wbb)))
  }

  saveRDS(updated_tbox_wbb, tbox_path_wbb)
  Sys.setFileTime(tbox_path_wbb, Sys.time())
} else {
  cat("  WBB team box: no new data to save.\n")
}

# ── Schedule ──────────────────────────────────────────────────────────────────
if (nrow(wbb$schedule) > 0) {
  sched_path_wbb <- glue::glue("BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Raw Data/schedule_daily_wbb_{cur_yr}.rds")

  if (file.exists(sched_path_wbb)) {
    existing_sched_wbb <- readRDS(sched_path_wbb)
    new_sched_wbb <- wbb$schedule %>%
      filter(game_id %!in% existing_sched_wbb$game_id)
    updated_sched_wbb <- bind_rows(existing_sched_wbb, new_sched_wbb)
    cat(sprintf("  WBB schedule: appended %d new rows\n", nrow(new_sched_wbb)))
  } else {
    updated_sched_wbb <- wbb$schedule
    cat(sprintf("  WBB schedule: created new cache with %d rows\n", nrow(updated_sched_wbb)))
  }

  saveRDS(updated_sched_wbb, sched_path_wbb)
  Sys.setFileTime(sched_path_wbb, Sys.time())
} else {
  cat("  WBB schedule: no new data to save.\n")
}


# ─────────────────────────────────────────────────────────────────────────────
# Summary
# ─────────────────────────────────────────────────────────────────────────────
cat("\n\n══════════════════════════════════════════════════════════════════════════\n")
cat("Daily pull complete.\n")
cat(sprintf("Date range pulled: %s to %s\n", pull_start, pull_end))
cat("\nFresh daily cache files saved:\n")
cat(sprintf("  MBB PBP:         VRGL/Stats/.../pbp_raw_daily_%s.rds\n",         cur_yr))
cat(sprintf("  MBB Player box:  VRGL/Stats/.../roster_daily_%s.rds\n",          cur_yr))
cat(sprintf("  MBB Team box:    VRGL/Stats/.../team_box_daily_%s.rds\n",        cur_yr))
cat(sprintf("  MBB Schedule:    VRGL/Stats/.../schedule_daily_%s.rds\n",        cur_yr))
cat(sprintf("  WBB PBP:         BTRC/Stats/.../pbp_raw_daily_wbb_%s.rds\n",     cur_yr))
cat(sprintf("  WBB Player box:  BTRC/Stats/.../roster_daily_wbb_%s.rds\n",      cur_yr))
cat(sprintf("  WBB Team box:    BTRC/Stats/.../team_box_daily_wbb_%s.rds\n",    cur_yr))
cat(sprintf("  WBB Schedule:    BTRC/Stats/.../schedule_daily_wbb_%s.rds\n",    cur_yr))
cat("\nNext step: run the main pipeline scripts (1 through 4) as normal.\n")
cat("In those scripts, replace the broken hoopR/wehoop load_* calls with\n")
cat("readRDS() pointing to the daily cache files above (see comments in\n")
cat("pull_daily_mbb.R / pull_daily_wbb.R for the exact swap instructions).\n")
cat("══════════════════════════════════════════════════════════════════════════\n")
