# =============================================================================
# load_mbb_team_box_espn.R
#
# Direct ESPN API replacement for hoopR::load_mbb_team_box(seasons = 2026).
#
# HOW THE ORIGINAL WORKS:
#   load_mbb_team_box() downloads a pre-built parquet from sportsdataverse-data.
#   That parquet is built by espn_mbb_02_team_box_creation.R, which loops over
#   game IDs from the schedule, calls espn_mbb_game_all(game_id), and pulls
#   the $Team element — which itself calls the ESPN summary endpoint:
#     https://site.web.api.espn.com/apis/site/v2/sports/basketball/
#       mens-college-basketball/summary?event={game_id}
#   and parses boxscore$teams[[i]]$statistics into a tidy row.
#
# THIS REPLACEMENT:
#   - Hits the same ESPN summary endpoint directly
#   - Produces the exact 59-column schema from hoopR::load_mbb_team_box()
#   - Accepts a vector of game IDs (from your schedule) OR a date range
#   - Merges cleanly with existing hoopR team box data via bind_rows
#
# OUTPUT SCHEMA (59 columns, 2 rows per game — one per team):
#   game_id, season, season_type, game_date, game_date_time,
#   team_id, team_uid, team_slug, team_location, team_name,
#   team_abbreviation, team_display_name, team_short_display_name,
#   team_color, team_alternate_color, team_logo,
#   team_home_away, team_score, team_winner,
#   assists, blocks, defensive_rebounds, fast_break_points,
#   field_goal_pct, field_goals_made, field_goals_attempted,
#   flagrant_fouls, fouls, free_throw_pct, free_throws_made,
#   free_throws_attempted, largest_lead, lead_changes, lead_percentage,
#   offensive_rebounds, points_in_paint, steals, team_turnovers,
#   technical_fouls, three_point_field_goal_pct,
#   three_point_field_goals_made, three_point_field_goals_attempted,
#   total_rebounds, total_technical_fouls, total_turnovers,
#   turnover_points, turnovers,
#   opponent_team_id, opponent_team_uid, opponent_team_slug,
#   opponent_team_location, opponent_team_name, opponent_team_abbreviation,
#   opponent_team_display_name, opponent_team_short_display_name,
#   opponent_team_color, opponent_team_alternate_color,
#   opponent_team_logo, opponent_team_score
# =============================================================================

library(dplyr)
library(purrr)
library(httr)
library(jsonlite)
library(lubridate)
library(glue)

ESPN_SUMMARY_URL <- "https://site.web.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary"

# =============================================================================
# HELPERS
# =============================================================================

safe <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) default else x[[1]]
}
safe_int <- function(x) as.integer(safe(x, NA_integer_))
safe_chr <- function(x) as.character(safe(x, NA_character_))
safe_lgl <- function(x) as.logical(safe(x, NA))
safe_dbl <- function(x) as.numeric(safe(x, NA_real_))

# Build team slug from location + name (hoopR format: "duke-blue-devils")
make_slug <- function(location, name) {
  if (is.na(location) || is.na(name)) return(NA_character_)
  slug <- paste(location, name)
  slug <- tolower(slug)
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("^-|-$", "", slug)
  slug
}

# ESPN only populates displayValue, never value, for this endpoint.
# Combined stats like "fieldGoalsMade-fieldGoalsAttempted" have displayValue "28-62"
# and must be split on "-" to get the two integers.
# Pct stats like "fieldGoalPct" have displayValue "45.2" and are parsed as numeric.
# Everything else is a plain integer stored in displayValue.

# Maps ESPN stat name -> how to handle it:
#   "split_made"  : displayValue is "made-attempted", extract [1] as integer
#   "split_att"   : displayValue is "made-attempted", extract [2] as integer
#   "int"         : displayValue is a plain integer string
#   "dbl"         : displayValue is a decimal number (pct)
#   "chr"         : keep displayValue as character
STAT_PARSE <- list(
  "fieldGoalsMade-fieldGoalsAttempted"             = list(made = "field_goals_made",
                                                          att  = "field_goals_attempted"),
  "threePointFieldGoalsMade-threePointFieldGoalsAttempted" = list(made = "three_point_field_goals_made",
                                                                  att  = "three_point_field_goals_attempted"),
  "freeThrowsMade-freeThrowsAttempted"             = list(made = "free_throws_made",
                                                          att  = "free_throws_attempted"),
  "fieldGoalPct"                                   = list(dbl  = "field_goal_pct"),
  "threePointFieldGoalPct"                         = list(dbl  = "three_point_field_goal_pct"),
  "freeThrowPct"                                   = list(dbl  = "free_throw_pct"),
  "totalRebounds"                                  = list(int  = "total_rebounds"),
  "offensiveRebounds"                              = list(int  = "offensive_rebounds"),
  "defensiveRebounds"                              = list(int  = "defensive_rebounds"),
  "assists"                                        = list(int  = "assists"),
  "steals"                                         = list(int  = "steals"),
  "blocks"                                         = list(int  = "blocks"),
  "turnovers"                                      = list(int  = "turnovers"),
  "teamTurnovers"                                  = list(int  = "team_turnovers"),
  "totalTurnovers"                                 = list(int  = "total_turnovers"),
  "technicalFouls"                                 = list(int  = "technical_fouls"),
  "totalTechnicalFouls"                            = list(int  = "total_technical_fouls"),
  "flagrantFouls"                                  = list(int  = "flagrant_fouls"),
  "fouls"                                          = list(int  = "fouls"),
  "turnoverPoints"                                 = list(chr  = "turnover_points"),
  "fastBreakPoints"                                = list(chr  = "fast_break_points"),
  "pointsInPaint"                                  = list(chr  = "points_in_paint"),
  "largestLead"                                    = list(chr  = "largest_lead"),
  "leadChanges"                                    = list(chr  = "lead_changes"),
  "leadPercentage"                                 = list(chr  = "lead_percentage")
)

# Parse the statistics array from one team's boxscore into a flat named list
parse_stats <- function(stats) {
  out <- list()
  if (is.null(stats) || length(stats) == 0) return(out)
  
  for (s in stats) {
    sname <- safe_chr(s$name)
    sdisp <- safe_chr(s$displayValue)
    rule  <- STAT_PARSE[[sname]]
    if (is.null(rule)) next
    
    if (!is.null(rule$made)) {
      # Split "28-62" into made=28, attempted=62
      parts <- strsplit(sdisp, "-")[[1]]
      out[[rule$made]] <- as.integer(parts[1])
      out[[rule$att]]  <- as.integer(parts[2])
    } else if (!is.null(rule$int)) {
      out[[rule$int]]  <- as.integer(sdisp)
    } else if (!is.null(rule$dbl)) {
      out[[rule$dbl]]  <- as.numeric(sdisp)
    } else if (!is.null(rule$chr)) {
      out[[rule$chr]]  <- as.character(sdisp)
    }
  }
  out
}


# =============================================================================
# CORE: Parse one ESPN summary JSON into a 2-row team box tibble
# =============================================================================

#' Parse ESPN summary JSON into hoopR-schema team box rows
#'
#' @param json   Parsed JSON from the ESPN summary endpoint
#' @param game_id  Integer game ID (used for error messages)
#' @return tibble with 2 rows (one per team) and 59 columns
parse_team_box_json <- function(json, game_id) {
  
  # ── Extract game metadata from header ────────────────────────────────────────
  # ESPN summary has a 'header' with competition info
  header <- json$header
  comp   <- if (!is.null(header)) header$competitions[[1]] else NULL
  
  # Fallback: also look in gameInfo
  game_info <- json$gameInfo
  
  # Date / time from header
  date_raw       <- safe_chr(comp$date %||% header$season$year)
  date_raw       <- if (!is.null(comp)) safe_chr(comp$date) else NA_character_
  game_date      <- tryCatch(as.Date(substr(date_raw, 1, 10)), error = function(e) NA_Date_())
  game_date_time <- tryCatch(
    lubridate::ymd_hms(date_raw, quiet = TRUE) %>% lubridate::with_tz("America/New_York"),
    error = function(e) as.POSIXct(NA)
  )
  
  season      <- safe_int(header$season$year)
  season_type <- safe_int(header$season$type)
  
  # ── Extract boxscore teams ────────────────────────────────────────────────────
  bs    <- json$boxscore
  teams <- bs$teams
  
  if (is.null(teams) || length(teams) < 2) {
    message(glue("  [WARN] game {game_id}: boxscore has <2 teams"))
    return(tibble())
  }
  
  # ── Also pull competitor info from header (has score, winner, homeAway) ──────
  competitors <- if (!is.null(comp)) comp$competitors else list()
  get_comp <- function(team_id_str) {
    idx <- which(sapply(competitors, function(x) {
      as.character(safe_chr(x$team$id)) == as.character(team_id_str)
    }))
    if (length(idx) > 0) competitors[[idx[1]]] else NULL
  }
  
  # ── Process each team ─────────────────────────────────────────────────────────
  team_rows <- map(seq_along(teams), function(i) {
    
    t    <- teams[[i]]
    team <- t$team
    
    # Team identity
    t_id      <- safe_int(team$id)
    t_uid     <- safe_chr(team$uid)
    t_loc     <- safe_chr(team$location)
    t_name    <- safe_chr(team$name)
    t_abbrev  <- safe_chr(team$abbreviation)
    t_disp    <- safe_chr(team$displayName)
    t_short   <- safe_chr(team$shortDisplayName)
    t_color   <- safe_chr(team$color)
    t_alt     <- safe_chr(team$alternateColor)
    t_logo    <- tryCatch(safe_chr(team$logos[[1]]$href), error = function(e) NA_character_)
    t_slug    <- make_slug(t_loc, t_name)
    
    # Competitor info (score, winner, home/away)
    cx         <- get_comp(t_id)
    t_home_away<- if (!is.null(cx)) safe_chr(cx$homeAway) else NA_character_
    t_score    <- if (!is.null(cx)) safe_int(cx$score)    else NA_integer_
    t_winner   <- if (!is.null(cx)) safe_lgl(cx$winner)   else NA
    
    # ── Parse statistics array ──────────────────────────────────────────────────
    st <- parse_stats(t$statistics)
    
    get_chr <- function(nm) { v <- st[[nm]]; if (is.null(v)) NA_character_ else as.character(v) }
    get_int <- function(nm) { v <- st[[nm]]; if (is.null(v)) NA_integer_   else as.integer(v)   }
    get_dbl <- function(nm) { v <- st[[nm]]; if (is.null(v)) NA_real_      else as.numeric(v)   }
    
    
    list(
      team_id                           = t_id,
      team_uid                          = t_uid,
      team_slug                         = t_slug,
      team_location                     = t_loc,
      team_name                         = t_name,
      team_abbreviation                 = t_abbrev,
      team_display_name                 = t_disp,
      team_short_display_name           = t_short,
      team_color                        = t_color,
      team_alternate_color              = t_alt,
      team_logo                         = t_logo,
      team_home_away                    = t_home_away,
      team_score                        = t_score,
      team_winner                       = t_winner,
      assists                           = get_int("assists"),
      blocks                            = get_int("blocks"),
      defensive_rebounds                = get_int("defensive_rebounds"),
      fast_break_points                 = get_chr("fast_break_points"),
      field_goal_pct                    = get_dbl("field_goal_pct"),
      field_goals_made                  = get_int("field_goals_made"),
      field_goals_attempted             = get_int("field_goals_attempted"),
      flagrant_fouls                    = get_int("flagrant_fouls"),
      fouls                             = get_int("fouls"),
      free_throw_pct                    = get_dbl("free_throw_pct"),
      free_throws_made                  = get_int("free_throws_made"),
      free_throws_attempted             = get_int("free_throws_attempted"),
      largest_lead                      = get_chr("largest_lead"),
      lead_changes                      = get_chr("lead_changes"),
      lead_percentage                   = get_chr("lead_percentage"),
      offensive_rebounds                = get_int("offensive_rebounds"),
      points_in_paint                   = get_chr("points_in_paint"),
      steals                            = get_int("steals"),
      team_turnovers                    = get_int("team_turnovers"),
      technical_fouls                   = get_int("technical_fouls"),
      three_point_field_goal_pct        = get_dbl("three_point_field_goal_pct"),
      three_point_field_goals_made      = get_int("three_point_field_goals_made"),
      three_point_field_goals_attempted = get_int("three_point_field_goals_attempted"),
      total_rebounds                    = get_int("total_rebounds"),
      total_technical_fouls             = get_int("total_technical_fouls"),
      total_turnovers                   = get_int("total_turnovers"),
      turnover_points                   = get_chr("turnover_points"),
      turnovers                         = get_int("turnovers")
    )
  })
  
  # ── Cross-populate opponent fields (hoopR: each row gets the other team) ──────
  # team_rows[[1]] gets opponent info from team_rows[[2]] and vice versa
  build_row <- function(my_idx, opp_idx) {
    me  <- team_rows[[my_idx]]
    opp <- team_rows[[opp_idx]]
    
    tibble(
      game_id                           = as.integer(game_id),
      season                            = season,
      season_type                       = season_type,
      game_date                         = game_date,
      game_date_time                    = game_date_time,
      team_id                           = me$team_id,
      team_uid                          = me$team_uid,
      team_slug                         = me$team_slug,
      team_location                     = me$team_location,
      team_name                         = me$team_name,
      team_abbreviation                 = me$team_abbreviation,
      team_display_name                 = me$team_display_name,
      team_short_display_name           = me$team_short_display_name,
      team_color                        = me$team_color,
      team_alternate_color              = me$team_alternate_color,
      team_logo                         = me$team_logo,
      team_home_away                    = me$team_home_away,
      team_score                        = me$team_score,
      team_winner                       = me$team_winner,
      assists                           = me$assists,
      blocks                            = me$blocks,
      defensive_rebounds                = me$defensive_rebounds,
      fast_break_points                 = me$fast_break_points,
      field_goal_pct                    = me$field_goal_pct,
      field_goals_made                  = me$field_goals_made,
      field_goals_attempted             = me$field_goals_attempted,
      flagrant_fouls                    = me$flagrant_fouls,
      fouls                             = me$fouls,
      free_throw_pct                    = me$free_throw_pct,
      free_throws_made                  = me$free_throws_made,
      free_throws_attempted             = me$free_throws_attempted,
      largest_lead                      = me$largest_lead,
      lead_changes                      = me$lead_changes,
      lead_percentage                   = me$lead_percentage,
      offensive_rebounds                = me$offensive_rebounds,
      points_in_paint                   = me$points_in_paint,
      steals                            = me$steals,
      team_turnovers                    = me$team_turnovers,
      technical_fouls                   = me$technical_fouls,
      three_point_field_goal_pct        = me$three_point_field_goal_pct,
      three_point_field_goals_made      = me$three_point_field_goals_made,
      three_point_field_goals_attempted = me$three_point_field_goals_attempted,
      total_rebounds                    = me$total_rebounds,
      total_technical_fouls             = me$total_technical_fouls,
      total_turnovers                   = me$total_turnovers,
      turnover_points                   = me$turnover_points,
      turnovers                         = me$turnovers,
      # Opponent columns
      opponent_team_id                  = opp$team_id,
      opponent_team_uid                 = opp$team_uid,
      opponent_team_slug                = opp$team_slug,
      opponent_team_location            = opp$team_location,
      opponent_team_name                = opp$team_name,
      opponent_team_abbreviation        = opp$team_abbreviation,
      opponent_team_display_name        = opp$team_display_name,
      opponent_team_short_display_name  = opp$team_short_display_name,
      opponent_team_color               = opp$team_color,
      opponent_team_alternate_color     = opp$team_alternate_color,
      opponent_team_logo                = opp$team_logo,
      opponent_team_score               = opp$team_score
    )
  }
  
  bind_rows(build_row(1, 2), build_row(2, 1))
}

# NULL coalescing operator (base R doesn't have one)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b


# =============================================================================
# FETCH: single game_id → 2-row tibble
# =============================================================================

#' Fetch and parse team box score for a single game from ESPN API
#'
#' @param game_id  Integer ESPN game ID
#' @return tibble with 2 rows (one per team), 59 columns
fetch_team_box_game <- function(game_id) {
  url <- glue("{ESPN_SUMMARY_URL}?event={game_id}")
  tryCatch({
    res <- httr::GET(
      url,
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 (compatible; R hoopR-replacement)",
        "Accept"     = "application/json"
      ),
      httr::timeout(20)
    )
    if (httr::status_code(res) != 200) {
      message(glue("  [WARN] HTTP {httr::status_code(res)} for game {game_id}"))
      return(tibble())
    }
    json <- jsonlite::fromJSON(
      httr::content(res, as = "text", encoding = "UTF-8"),
      simplifyVector = TRUE, simplifyDataFrame = FALSE
    )
    parse_team_box_json(json, game_id)
  }, error = function(e) {
    message(glue("  [ERROR] game {game_id}: {conditionMessage(e)}"))
    tibble()
  })
}


# =============================================================================
# MAIN: load_mbb_team_box_espn()
# Drop-in replacement for hoopR::load_mbb_team_box(seasons = 2026)
# =============================================================================

#' Load MBB team box scores directly from ESPN API
#'
#' @description
#' Pulls team box scores for a set of game IDs by hitting the ESPN summary
#' endpoint directly, bypassing the stale sportsdataverse-data repo.
#'
#' Typical workflow:
#'   1. Pull your schedule (which has game_ids) via load_mbb_schedule_espn()
#'   2. Filter to STATUS_FINAL games on the dates you need
#'   3. Pass those game_ids to this function
#'
#' @param game_ids  Integer vector of ESPN game IDs to pull.
#'                  If NULL, falls back to pulling all final games in the
#'                  provided schedule for the given date range.
#' @param schedule  Optional tibble from load_mbb_schedule_espn() or
#'                  hoopR::load_mbb_schedule(). Used to resolve game_ids from
#'                  a date range when game_ids is NULL.
#' @param start_date  "YYYY-MM-DD". Used with schedule to filter by date range.
#' @param end_date    "YYYY-MM-DD". Defaults to today.
#' @param delay_sec   Pause between requests. Default 0.5s (summary API is heavier).
#' @param verbose     Print progress messages.
#'
#' @return tibble with same 59-column schema as hoopR::load_mbb_team_box()
load_mbb_team_box_espn <- function(
    game_ids   = NULL,
    schedule   = NULL,
    start_date = NULL,
    end_date   = NULL,
    delay_sec  = 0.5,
    verbose    = TRUE
) {
  
  # ── Resolve game IDs ─────────────────────────────────────────────────────────
  if (is.null(game_ids)) {
    if (is.null(schedule)) {
      stop("Provide either game_ids or a schedule tibble.")
    }
    sched <- schedule
    if (!is.null(start_date)) sched <- sched %>% filter(game_date >= as.Date(start_date))
    if (!is.null(end_date))   sched <- sched %>% filter(game_date <= as.Date(end_date))
    
    # Only pull games that are final
    status_col <- intersect(names(sched), c("status_type_name", "status_name"))[1]
    if (!is.na(status_col)) {
      sched <- sched %>% filter(.data[[status_col]] == "STATUS_FINAL")
    }
    
    game_ids <- unique(sched$game_id)
    
    if (length(game_ids) == 0) {
      warning("load_mbb_team_box_espn(): no STATUS_FINAL games in schedule for given date range.")
      return(tibble())
    }
  }
  
  game_ids <- as.integer(unique(game_ids))
  n        <- length(game_ids)
  
  if (verbose) message(glue("load_mbb_team_box_espn(): fetching {n} games..."))
  
  all_rows <- map_dfr(seq_along(game_ids), function(i) {
    gid <- game_ids[[i]]
    if (verbose && (i == 1 || i %% 50 == 0 || i == n)) {
      message(glue("  {i}/{n}: game_id={gid}"))
    }
    if (i > 1) Sys.sleep(delay_sec)
    fetch_team_box_game(gid)
  })
  
  if (nrow(all_rows) == 0) {
    warning("load_mbb_team_box_espn(): no rows returned. Check ESPN API connectivity.")
    return(tibble())
  }
  
  all_rows <- all_rows %>%
    distinct(game_id, team_id, .keep_all = TRUE) %>%
    arrange(game_date, game_id, team_home_away)
  
  if (verbose) message(glue(
    "Done: {n_distinct(all_rows$game_id)} games, ",
    "{nrow(all_rows)} team rows, ",
    "{min(all_rows$game_date, na.rm=TRUE)} to {max(all_rows$game_date, na.rm=TRUE)}"
  ))
  
  all_rows
}


# =============================================================================
# MERGE: combine existing hoopR team_box with fresh ESPN pull
# =============================================================================

#' Merge existing hoopR team box with new ESPN rows
#'
#' New rows replace existing for the same (game_id, team_id).
merge_mbb_team_box <- function(existing, new_rows) {
  if (is.null(existing) || nrow(existing) == 0) return(new_rows)
  if (is.null(new_rows)  || nrow(new_rows)  == 0) return(existing)
  
  combined <- existing %>%
    filter(!(paste(game_id, team_id) %in% paste(new_rows$game_id, new_rows$team_id))) %>%
    bind_rows(new_rows) %>%
    arrange(game_date, game_id, team_home_away)
  
  message(glue(
    "merge_mbb_team_box(): {nrow(existing)} existing + ",
    "{nrow(new_rows)} new/updated = {nrow(combined)} total rows"
  ))
  combined
}


# =============================================================================
# DIAGNOSTIC: inspect raw ESPN summary JSON for a single game
# =============================================================================

#' Print structure of ESPN summary JSON for a game
#' Useful for debugging stat name mismatches or missing fields.
#'
#' @param game_id  Integer ESPN game ID
diag_team_box <- function(game_id) {
  url <- glue("{ESPN_SUMMARY_URL}?event={game_id}")
  message("Fetching: ", url)
  res  <- httr::GET(url, httr::add_headers("User-Agent" = "Mozilla/5.0"), httr::timeout(20))
  message("HTTP status: ", httr::status_code(res))
  json <- jsonlite::fromJSON(
    httr::content(res, as = "text", encoding = "UTF-8"),
    simplifyVector = TRUE, simplifyDataFrame = FALSE
  )
  message("Top-level keys: ", paste(names(json), collapse = ", "))
  bs <- json$boxscore
  message("Boxscore keys: ", paste(names(bs), collapse = ", "))
  teams <- bs$teams
  if (!is.null(teams) && length(teams) >= 1) {
    t1 <- teams[[1]]
    message("Team 1: ", t1$team$displayName)
    message("Statistics names: ", paste(sapply(t1$statistics, function(s) s$name), collapse = ", "))
    message("--- First 5 stats ---")
    for (s in t1$statistics[1:min(5, length(t1$statistics))]) {
      message(sprintf("  %-45s val=%-10s disp=%s", s$name, s$value %||% "NA", s$displayValue %||% "NA"))
    }
  }
  invisible(json)
}


# =============================================================================
# USAGE EXAMPLES
# =============================================================================

# ── Option A: Pull specific game IDs (fastest, most targeted) ─────────────────
# new_game_ids <- c(401851750L, 401851532L, 401851491L)
# team_box_new <- load_mbb_team_box_espn(game_ids = new_game_ids)

# ── Option B: Pull all final games from a date range using your schedule ───────
# source("load_mbb_schedule_espn.R")
# schedule_full  <- readRDS("path/to/schedule_daily_2026.rds")   # your saved schedule
# team_box_new   <- load_mbb_team_box_espn(
#   schedule   = schedule_full,
#   start_date = "2026-03-07",
#   end_date   = as.character(Sys.Date() - 1)
# )

# ── Option C: Merge with existing hoopR baseline ──────────────────────────────
# team_box_base  <- hoopR::load_mbb_team_box(seasons = 2026)   # stale through ~Mar 6
# team_box_full  <- merge_mbb_team_box(team_box_base, team_box_new)
# saveRDS(team_box_full, "path/to/team_box_daily_2026.rds")

# ── Option D: Daily update (add to run_daily_pull.R) ─────────────────────────
# yesterday_ids  <- schedule_full %>%
#   filter(game_date == Sys.Date() - 1, status_type_name == "STATUS_FINAL") %>%
#   pull(game_id)
# team_box_today <- load_mbb_team_box_espn(game_ids = yesterday_ids)
# team_box_full  <- merge_mbb_team_box(team_box_full, team_box_today)
# saveRDS(team_box_full, team_box_path)

# ── Diagnostic: check a specific game ─────────────────────────────────────────
# diag_team_box(401851750)