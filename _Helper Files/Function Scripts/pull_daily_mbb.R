# ─────────────────────────────────────────────────────────────────────────────
# pull_daily_mbb.R
#
# Pulls MBB data DIRECTLY from ESPN's public API using jsonlite.
# Does NOT use hoopR at all — use this when hoopR is failing to return data.
#
# ESPN endpoints used:
#   Scoreboard:  site.api.espn.com/.../mens-college-basketball/scoreboard?dates=YYYYMMDD
#   Game summary: site.api.espn.com/.../mens-college-basketball/summary?event={game_id}
#     (summary contains PBP, team box, and player box in one response)
#
# Requires: jsonlite, tidyverse, purrr
#
# USAGE:
#   source("_Helper Files/Function Scripts/pull_daily_mbb.R")
#
#   # Catch-up for multiple days:
#   result <- pull_mbb_daily_data("2026-03-07", "2026-03-11")
#
#   # Single day (e.g. yesterday):
#   result <- pull_mbb_daily_data(Sys.Date() - 1)
#
#   pbp        <- result$pbp
#   team_box   <- result$team_box
#   player_box <- result$player_box
#   schedule   <- result$schedule
# ─────────────────────────────────────────────────────────────────────────────

library(jsonlite)
library(tidyverse)
library(purrr)

# ESPN API base
.MBB_API    <- "https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball"
.MBB_GROUPS <- "50"   # ESPN group ID for all NCAA Division I men's basketball


# ─────────────────────────────────────────────────────────────────────────────
# Internal helpers
# ─────────────────────────────────────────────────────────────────────────────

# Null-safe coercion
.chr <- function(x) { if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]]) }
.int <- function(x) { suppressWarnings(as.integer(.chr(x))) }
.num <- function(x) { suppressWarnings(as.numeric(.chr(x))) }

# Date sequence
.date_seq <- function(date_start, date_end = NULL) {
  if (is.null(date_end)) date_end <- date_start
  seq(as.Date(date_start), as.Date(date_end), by = "day")
}

# ESPN API GET with retry
.espn_get <- function(url, retries = 3) {
  for (i in seq_len(retries)) {
    result <- tryCatch(
      jsonlite::read_json(url),
      error = function(e) {
        message(sprintf("    Attempt %d failed: %s", i, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(result)) return(result)
    if (i < retries) Sys.sleep(2 * i)
  }
  message(sprintf("    ESPN API gave up after %d attempts:\n    %s", retries, url))
  return(NULL)
}


# ─────────────────────────────────────────────────────────────────────────────
# Internal: get completed game IDs for a single date from the scoreboard
# ─────────────────────────────────────────────────────────────────────────────
.get_mbb_game_ids_for_date <- function(date) {
  d_str <- format(as.Date(date), "%Y%m%d")
  url   <- sprintf("%s/scoreboard?dates=%s&groups=%s&limit=1000", .MBB_API, d_str, .MBB_GROUPS)
  data  <- .espn_get(url)

  if (is.null(data) || is.null(data$events) || length(data$events) == 0) {
    message(sprintf("  [MBB] No games found for %s", date))
    return(character(0))
  }

  ids <- unlist(Filter(Negate(is.null), lapply(data$events, function(e) {
    if (isTRUE(e$status$type$completed)) as.character(e$id) else NULL
  })))

  message(sprintf("  [MBB] %s: %d completed game(s)", date, length(ids)))
  return(ids)
}


# ─────────────────────────────────────────────────────────────────────────────
# Internal: parse ESPN summary JSON → schedule row (one row per game)
# ─────────────────────────────────────────────────────────────────────────────
.parse_mbb_schedule_row <- function(event) {
  comp      <- if (!is.null(event$competitions)) event$competitions[[1]] else list()
  home_id   <- NA_character_; away_id   <- NA_character_
  home_scr  <- NA_integer_;   away_scr  <- NA_integer_

  for (cr in comp$competitors) {
    if (identical(cr$homeAway, "home")) {
      home_id  <- .chr(cr$team$id)
      home_scr <- suppressWarnings(as.integer(.chr(cr$score)))
    }
    if (identical(cr$homeAway, "away")) {
      away_id  <- .chr(cr$team$id)
      away_scr <- suppressWarnings(as.integer(.chr(cr$score)))
    }
  }

  data.frame(
    game_id                 = .chr(event$id),
    id                      = .chr(event$id),          # alias; scripts use both names
    game_date               = tryCatch(as.Date(substr(.chr(event$date), 1, 10)), error = function(e) NA),
    game_date_time          = .chr(event$date),
    home_id                 = home_id,
    away_id                 = away_id,
    home_score              = home_scr,
    away_score              = away_scr,
    neutral_site            = isTRUE(comp$neutralSite),
    venue_id                = .chr(comp$venue$id),
    tournament_id           = .chr(comp$tournament$id),
    conference_competition  = isTRUE(comp$conferenceCompetition),
    status_type_name        = .chr(event$status$type$name),
    status_type_description = .chr(event$status$type$description),
    status_type_completed   = isTRUE(event$status$type$completed),
    status_period           = .int(event$status$period),
    stringsAsFactors        = FALSE
  )
}


# ─────────────────────────────────────────────────────────────────────────────
# Internal: parse ESPN summary JSON → PBP data frame
# ─────────────────────────────────────────────────────────────────────────────
.parse_mbb_pbp <- function(data, game_id) {
  if (is.null(data$plays) || length(data$plays) == 0) return(NULL)

  # Season, date, and team info from header
  season_year <- .int(data$header$season$year)
  season_type <- .int(data$header$season$type)
  home_id     <- NA_character_
  away_id     <- NA_character_
  game_date   <- as.Date(NA)

  comps <- data$header$competitions
  if (!is.null(comps) && length(comps) > 0) {
    comp <- comps[[1]]
    game_date <- tryCatch(as.Date(substr(.chr(comp$date), 1, 10)), error = function(e) as.Date(NA))
    for (cr in comp$competitors) {
      if (identical(cr$homeAway, "home")) home_id <- .chr(cr$id)
      if (identical(cr$homeAway, "away")) away_id <- .chr(cr$id)
    }
  }

  rows <- lapply(data$plays, function(p) {
    # Clock
    clk_str   <- .chr(p$clock$displayValue)
    if (is.na(clk_str)) clk_str <- "0:00"
    clk_parts <- strsplit(clk_str, ":")[[1]]
    clk_min   <- suppressWarnings(as.numeric(clk_parts[1]))
    clk_sec   <- suppressWarnings(as.numeric(if (length(clk_parts) >= 2) clk_parts[2] else "0"))

    # Athletes
    parts <- p$participants
    ath1  <- if (!is.null(parts) && length(parts) >= 1) .chr(parts[[1]]$athlete$id) else NA_character_
    ath2  <- if (!is.null(parts) && length(parts) >= 2) .chr(parts[[2]]$athlete$id) else NA_character_

    # Coordinates
    cx <- .num(p$coordinate$x)
    cy <- .num(p$coordinate$y)

    # score_value = shot TYPE value (1/2/3), NOT whether the shot was made.
    # ESPN's scoreValue is 0 for missed shots, so infer from type_id when needed.
    # Pipeline scripts use score_value as the shot face-value for xFG calculations.
    tid_raw   <- .chr(p$type$id)
    sv_raw    <- .int(p$scoreValue)
    is_shoot  <- isTRUE(p$shootingPlay)
    score_val <- if (!is_shoot) {
      0L
    } else if (!is.na(sv_raw) && sv_raw > 0) {
      sv_raw                                          # ESPN gave us a value; trust it
    } else if (grepl("558|574", tid_raw)) {
      3L                                              # 3-point attempt type IDs
    } else if (grepl("540", tid_raw)) {
      1L                                              # Free throw type IDs
    } else {
      2L                                              # Default: 2-point attempt
    }

    data.frame(
      game_id                      = as.character(game_id),
      game_date                    = game_date,
      game_play_number             = .int(p$sequenceNumber),
      period_number                = .int(p$period$number),
      period_display_value         = .chr(p$period$displayValue),
      clock_display_value          = clk_str,
      clock_minutes                = clk_min,
      clock_seconds                = clk_sec,
      type_id                      = tid_raw,
      type_text                    = .chr(p$type$text),
      text                         = .chr(p$text),
      shooting_play                = is_shoot,
      scoring_play                 = isTRUE(p$scoring),
      score_value                  = score_val,
      team_id                      = .chr(p$team$id),
      home_team_id                 = home_id,
      away_team_id                 = away_id,
      home_score                   = suppressWarnings(as.integer(.chr(p$homeScore))),
      away_score                   = suppressWarnings(as.integer(.chr(p$awayScore))),
      athlete_id_1                 = ath1,
      athlete_id_2                 = ath2,
      coordinate_x_raw             = cx,
      coordinate_y_raw             = cy,
      coordinate_x                 = cx,
      coordinate_y                 = cy,
      start_game_seconds_remaining = NA_real_,
      season                       = season_year,
      season_type                  = season_type,
      stringsAsFactors             = FALSE
    )
  })

  do.call(rbind, rows)
}


# ─────────────────────────────────────────────────────────────────────────────
# Internal: parse ESPN summary JSON → team box score data frame
# ─────────────────────────────────────────────────────────────────────────────
.parse_mbb_team_box <- function(data, game_id) {
  bx <- data$boxscore
  if (is.null(bx) || is.null(bx$teams) || length(bx$teams) == 0) return(NULL)

  # Game date and scores from header (more reliable than boxscore stats)
  game_date_str <- NA_character_
  comp_scores   <- list()
  comps <- data$header$competitions
  if (!is.null(comps) && length(comps) > 0) {
    comp <- comps[[1]]
    game_date_str <- .chr(comp$date)
    for (cr in comp$competitors) {
      comp_scores[[.chr(cr$id)]] <- suppressWarnings(as.integer(.chr(cr$score)))
    }
  }
  game_date <- tryCatch(as.Date(substr(game_date_str, 1, 10)), error = function(e) as.Date(NA))

  teams    <- bx$teams
  team_ids <- sapply(teams, function(t) .chr(t$team$id))

  rows <- lapply(seq_along(teams), function(i) {
    t      <- teams[[i]]
    tid    <- .chr(t$team$id)
    opp_id <- team_ids[team_ids != tid][1]

    # Build named stats lookup from the statistics array
    sv <- character(0)
    if (!is.null(t$statistics)) {
      for (s in t$statistics) {
        nm <- .chr(s$name)
        if (!is.na(nm)) sv[nm] <- .chr(s$displayValue)
      }
    }

    # Return numeric stat by name; NA if not present
    gstat <- function(nm) suppressWarnings(as.numeric(sv[nm]))

    # Parse "made-attempted" combined strings (e.g. "28-62")
    gsplit <- function(nm, idx) {
      raw <- sv[nm]
      if (is.na(raw) || is.null(raw) || nchar(raw) == 0) return(NA_real_)
      pts <- strsplit(raw, "-")[[1]]
      if (length(pts) >= idx) suppressWarnings(as.numeric(pts[idx])) else NA_real_
    }

    # Helper: try a list of stat names and return the first non-NA
    gfirst <- function(...) {
      for (nm in c(...)) {
        v <- gstat(nm)
        if (!is.na(v)) return(v)
      }
      NA_real_
    }

    # Field goals
    fg_made <- gfirst("fieldGoalsMade")
    fg_att  <- gfirst("fieldGoalsAttempted")
    if (is.na(fg_made)) fg_made <- gsplit("fieldGoalsMadeFieldGoalsAttempted", 1)
    if (is.na(fg_att))  fg_att  <- gsplit("fieldGoalsMadeFieldGoalsAttempted", 2)

    # Three-pointers — ESPN uses several possible key names
    tp_made <- gfirst("threePointFieldGoalsMade", "threePointsMade")
    tp_att  <- gfirst("threePointFieldGoalsAttempted", "threePointsAttempted")
    if (is.na(tp_made)) tp_made <- gsplit("threePointFieldGoalsMadeThreePointFieldGoalsAttempted", 1)
    if (is.na(tp_att))  tp_att  <- gsplit("threePointFieldGoalsMadeThreePointFieldGoalsAttempted", 2)
    if (is.na(tp_made)) tp_made <- gsplit("threePointMadeThreePointAttempted", 1)
    if (is.na(tp_att))  tp_att  <- gsplit("threePointMadeThreePointAttempted", 2)

    # Free throws
    ft_made <- gfirst("freeThrowsMade")
    ft_att  <- gfirst("freeThrowsAttempted")
    if (is.na(ft_made)) ft_made <- gsplit("freeThrowsMadeFreeThrowsAttempted", 1)
    if (is.na(ft_att))  ft_att  <- gsplit("freeThrowsMadeFreeThrowsAttempted", 2)

    # Score: prefer header competitors, fall back to stats
    score     <- comp_scores[[tid]]
    opp_score <- comp_scores[[opp_id]]
    if (is.null(score)     || is.na(score))     score     <- as.integer(gfirst("points"))
    if (is.null(opp_score) || is.na(opp_score)) opp_score <- NA_integer_

    data.frame(
      game_id                            = as.character(game_id),
      game_date                          = game_date,
      team_id                            = tid,
      team_location                      = .chr(t$team$location),
      team_name                          = .chr(t$team$name),
      team_display_name                  = .chr(t$team$displayName),
      team_score                         = as.integer(score),
      opponent_team_id                   = as.character(opp_id),
      opponent_team_score                = as.integer(opp_score),
      team_home_away                     = .chr(t$homeAway),
      field_goals_made                   = as.integer(fg_made),
      field_goals_attempted              = as.integer(fg_att),
      three_point_field_goals_made       = as.integer(tp_made),
      three_point_field_goals_attempted  = as.integer(tp_att),
      free_throws_made                   = as.integer(ft_made),
      free_throws_attempted              = as.integer(ft_att),
      offensive_rebounds                 = as.integer(gfirst("offensiveRebounds")),
      defensive_rebounds                 = as.integer(gfirst("defensiveRebounds")),
      rebounds                           = as.integer(gfirst("totalRebounds", "rebounds")),
      assists                            = as.integer(gfirst("assists")),
      steals                             = as.integer(gfirst("steals")),
      blocks                             = as.integer(gfirst("blocks")),
      turnovers                          = as.integer(gfirst("turnovers")),
      stringsAsFactors                   = FALSE
    )
  })

  do.call(rbind, rows)
}


# ─────────────────────────────────────────────────────────────────────────────
# Internal: parse ESPN summary JSON → player box (roster) data frame
# ─────────────────────────────────────────────────────────────────────────────
.parse_mbb_player_box <- function(data, game_id) {
  bx <- data$boxscore
  if (is.null(bx) || is.null(bx$players) || length(bx$players) == 0) return(NULL)

  rows <- list()
  for (team_data in bx$players) {
    tid <- .chr(team_data$team$id)
    if (is.null(team_data$statistics) || length(team_data$statistics) == 0) next

    for (stat_group in team_data$statistics) {
      if (is.null(stat_group$athletes) || length(stat_group$athletes) == 0) next

      for (ae in stat_group$athletes) {
        ath <- ae$athlete
        if (is.null(ath)) next

        rows[[length(rows) + 1]] <- data.frame(
          game_id                       = as.character(game_id),
          athlete_id                    = .chr(ath$id),
          athlete_display_name          = .chr(ath$displayName),
          athlete_jersey                = .chr(ath$jersey),
          athlete_headshot_href         = .chr(ath$headshot$href),
          athlete_position_abbreviation = .chr(ath$position$abbreviation),
          team_id                       = tid,
          stringsAsFactors              = FALSE
        )
      }
    }
  }

  if (length(rows) == 0) return(NULL)
  unique(do.call(rbind, rows))
}


# ─────────────────────────────────────────────────────────────────────────────
# Internal: pull and parse all data for a single game_id
# Returns a list with $pbp, $team_box, $player_box
# ─────────────────────────────────────────────────────────────────────────────
.pull_mbb_game <- function(game_id, sleep_secs = 0.5) {
  Sys.sleep(sleep_secs)
  url  <- sprintf("%s/summary?event=%s", .MBB_API, game_id)
  data <- .espn_get(url)

  if (is.null(data)) {
    message(sprintf("    [MBB] game %s: no data returned", game_id))
    return(NULL)
  }

  list(
    pbp        = tryCatch(.parse_mbb_pbp(data, game_id),        error = function(e) { message(sprintf("    [MBB] PBP parse failed for %s: %s", game_id, e$message)); NULL }),
    team_box   = tryCatch(.parse_mbb_team_box(data, game_id),   error = function(e) { message(sprintf("    [MBB] Team box parse failed for %s: %s", game_id, e$message)); NULL }),
    player_box = tryCatch(.parse_mbb_player_box(data, game_id), error = function(e) { message(sprintf("    [MBB] Player box parse failed for %s: %s", game_id, e$message)); NULL })
  )
}


# ═════════════════════════════════════════════════════════════════════════════
# PUBLIC FUNCTIONS
# ═════════════════════════════════════════════════════════════════════════════

# ─────────────────────────────────────────────────────────────────────────────
# pull_mbb_daily_data()
#
# Master function — pulls PBP, player box, team box, and schedule for all
# completed MBB games in the given date range in a single pass.
# Each game's summary endpoint is called once to populate all three tables.
#
# Returns a named list: $pbp, $player_box, $team_box, $schedule
# ─────────────────────────────────────────────────────────────────────────────
pull_mbb_daily_data <- function(date_start, date_end = NULL, sleep_secs = 0.5) {
  dates      <- .date_seq(date_start, date_end)
  date_label <- if (length(dates) == 1) as.character(dates[1]) else
    paste0(dates[1], " to ", dates[length(dates)])

  message(sprintf("\n===== MBB Daily Pull (direct ESPN API): %s =====", date_label))

  # ── 1. Get game IDs from scoreboard ───────────────────────────────────────
  all_ids <- unique(unlist(map(dates, .get_mbb_game_ids_for_date)))
  all_ids <- all_ids[!is.na(all_ids) & nchar(all_ids) > 0]

  if (length(all_ids) == 0) {
    message("  [MBB] No completed games found.")
    return(list(pbp = data.frame(), player_box = data.frame(),
                team_box = data.frame(), schedule = data.frame()))
  }
  message(sprintf("  [MBB] %d unique game(s) to pull.", length(all_ids)))

  # ── 2. Pull summary for each game (PBP + boxes in one request) ────────────
  message("  >> Pulling game summaries (PBP + box scores)...")
  game_results <- map(all_ids, .pull_mbb_game, sleep_secs = sleep_secs)
  game_results <- game_results[!sapply(game_results, is.null)]

  pbp        <- bind_rows(map(game_results, "pbp"))
  team_box   <- bind_rows(map(game_results, "team_box"))
  player_box <- bind_rows(map(game_results, "player_box"))

  message(sprintf("     PBP rows: %d | Team box rows: %d | Player box rows: %d",
                  nrow(pbp), nrow(team_box), nrow(player_box)))

  # ── 3. Pull schedule from scoreboard (already have the data, re-use it) ───
  message("  >> Building schedule from scoreboard...")
  sched_list <- map(dates, function(d) {
    d_str <- format(d, "%Y%m%d")
    url   <- sprintf("%s/scoreboard?dates=%s&groups=%s&limit=1000", .MBB_API, d_str, .MBB_GROUPS)
    data  <- .espn_get(url)
    if (is.null(data) || is.null(data$events) || length(data$events) == 0) return(NULL)
    tryCatch(
      bind_rows(lapply(data$events, .parse_mbb_schedule_row)),
      error = function(e) { message(sprintf("     Schedule parse failed for %s: %s", d, e$message)); NULL }
    )
  })
  schedule <- bind_rows(sched_list[!sapply(sched_list, is.null)])
  message(sprintf("     Schedule rows: %d", nrow(schedule)))

  message(sprintf("===== MBB Daily Pull complete =====\n"))

  list(pbp = pbp, player_box = player_box, team_box = team_box, schedule = schedule)
}


# ─────────────────────────────────────────────────────────────────────────────
# Individual convenience functions (each calls pull_mbb_daily_data internally)
# ─────────────────────────────────────────────────────────────────────────────
pull_mbb_pbp_by_date <- function(date_start, date_end = NULL, sleep_secs = 0.5) {
  pull_mbb_daily_data(date_start, date_end, sleep_secs)$pbp
}

pull_mbb_team_box_by_date <- function(date_start, date_end = NULL, sleep_secs = 0.5) {
  pull_mbb_daily_data(date_start, date_end, sleep_secs)$team_box
}

pull_mbb_player_box_by_date <- function(date_start, date_end = NULL, sleep_secs = 0.5) {
  pull_mbb_daily_data(date_start, date_end, sleep_secs)$player_box
}

pull_mbb_schedule_by_date <- function(date_start, date_end = NULL, sleep_secs = 0.5) {
  pull_mbb_daily_data(date_start, date_end, sleep_secs)$schedule
}
