# =============================================================================
# load_wbb_schedule_espn.R  (v2 — full column parity with wehoop)
#
# Replicates wehoop::load_wbb_schedule(seasons = 2026) by hitting ESPN's
# scoreboard API directly rather than the stale pre-built .rds file.
#
# Output schema matches the 86-column wehoop schedule exactly, including:
#   - hoopR naming conventions (home_id, home_location, home_name, etc.)
#   - Correct integer types for all ID fields
#   - Broadcast, status, venue, groups, linescores, and availability flag cols
# =============================================================================

library(dplyr)
library(purrr)
library(httr)
library(jsonlite)
library(lubridate)
library(glue)

ESPN_WBB_SCOREBOARD <- "http://site.api.espn.com/apis/site/v2/sports/basketball/womens-college-basketball/scoreboard"
WEHOOP_RAW_BASE  <- "https://raw.githubusercontent.com/sportsdataverse/wehoop-wbb-raw/main/wbb/json/final"

# =============================================================================
# HELPERS
# =============================================================================

safe <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) default else x[[1]]
}

safe_vec <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) default else x
}

safe_int  <- function(x) as.integer(safe(x, NA_integer_))
safe_chr  <- function(x) as.character(safe(x, NA_character_))
safe_lgl  <- function(x) as.logical(safe(x, NA))
safe_dbl  <- function(x) as.numeric(safe(x, NA_real_))


# =============================================================================
# CORE PARSER
# Each event in the ESPN JSON becomes one row matching hoopR's schema
# =============================================================================

parse_wbb_scoreboard_json <- function(json) {
  
  events <- json$events
  if (is.null(events) || length(events) == 0) return(tibble())
  
  map_dfr(seq_along(events), function(i) {
    ev   <- events[[i]]
    comp <- ev$competitions[[1]]
    
    # ── Top-level event fields ────────────────────────────────────────────────
    game_id    <- safe_int(ev$id)
    uid        <- safe_chr(ev$uid)
    date_raw   <- safe_chr(ev$date)    # "2026-03-07T04:30Z"
    start_date <- date_raw             # hoopR keeps raw string in start_date too
    
    game_date <- as.Date(substr(date_raw, 1, 10))
    game_date_time <- tryCatch(
      lubridate::ymd_hms(date_raw, quiet = TRUE) %>% lubridate::with_tz("America/New_York"),
      error = function(e) as.POSIXct(NA)
    )
    
    season      <- safe_int(ev$season$year)
    season_type <- safe_int(ev$season$type)
    
    # ── Competition-level flags ───────────────────────────────────────────────
    attendance         <- safe_int(comp$attendance)
    time_valid         <- safe_lgl(comp$timeValid)
    neutral_site       <- safe_lgl(comp$neutralSite)
    conference_comp    <- safe_lgl(comp$conferenceCompetition)
    play_by_play_avail <- safe_lgl(comp$playByPlayAvailable)
    recent             <- safe_lgl(comp$recent)
    
    # ── Broadcast ─────────────────────────────────────────────────────────────
    broadcasts       <- comp$broadcasts
    broadcast        <- NA_character_
    broadcast_market <- NA_character_
    broadcast_name   <- NA_character_
    if (!is.null(broadcasts) && length(broadcasts) > 0) {
      bc             <- broadcasts[[1]]
      broadcast_name <- tryCatch(paste(safe_vec(bc$names, ""), collapse = ", "),
                                 error = function(e) NA_character_)
      broadcast_market <- safe_chr(bc$market)
      broadcast        <- broadcast_name
    }
    
    # ── Highlights (stringified list, matches hoopR storage format) ───────────
    highlights <- tryCatch(
      as.character(jsonlite::toJSON(safe_vec(comp$highlights, list()), auto_unbox = TRUE)),
      error = function(e) "[]"
    )
    
    # ── Notes ─────────────────────────────────────────────────────────────────
    notes          <- comp$notes
    notes_type     <- NA_character_
    notes_headline <- NA_character_
    if (!is.null(notes) && length(notes) > 0) {
      notes_type     <- safe_chr(notes[[1]]$type)
      notes_headline <- safe_chr(notes[[1]]$headline)
    }
    
    # ── Competition type ──────────────────────────────────────────────────────
    type_id           <- safe_int(comp$type$id)
    type_abbreviation <- safe_chr(comp$type$abbreviation)
    
    # ── Venue ─────────────────────────────────────────────────────────────────
    venue_id            <- safe_int(comp$venue$id)
    venue_full_name     <- safe_chr(comp$venue$fullName)
    venue_address_city  <- safe_chr(comp$venue$address$city)
    venue_address_state <- safe_chr(comp$venue$address$state)
    venue_indoor        <- safe_lgl(comp$venue$indoor)
    
    # ── Status ────────────────────────────────────────────────────────────────
    status_clock            <- safe_dbl(comp$status$clock)
    status_display_clock    <- safe_chr(comp$status$displayClock)
    status_period           <- safe_dbl(comp$status$period)
    status_type_id          <- safe_int(comp$status$type$id)
    status_type_name        <- safe_chr(comp$status$type$name)
    status_type_state       <- safe_chr(comp$status$type$state)
    status_type_completed   <- safe_lgl(comp$status$type$completed)
    status_type_description <- safe_chr(comp$status$type$description)
    status_type_detail      <- safe_chr(comp$status$type$detail)
    status_type_short_detail<- safe_chr(comp$status$type$shortDetail)
    
    # alt_detail: "OT", "2OT", "3OT" etc. — NA if regulation final
    status_type_alt_detail <- tryCatch({
      sd <- status_type_short_detail
      if (!is.na(sd) && grepl("/OT", sd)) sub("Final/", "", sd) else NA_character_
    }, error = function(e) NA_character_)
    
    # ── Format ────────────────────────────────────────────────────────────────
    format_regulation_periods <- safe_dbl(comp$format$regulationPeriods)
    
    # ── Groups (conference) ───────────────────────────────────────────────────
    groups_id            <- safe_int(comp$groups$id)
    groups_name          <- safe_chr(comp$groups$name)
    groups_short_name    <- safe_chr(comp$groups$shortName)
    groups_is_conference <- safe_lgl(comp$groups$isConference)
    
    # ── Tournament ID ─────────────────────────────────────────────────────────
    # hoopR derives this from comp$series; not always present
    tournament_id <- tryCatch(safe_int(comp$series$type$id), error = function(e) NA_integer_)
    
    # ── Competitors ───────────────────────────────────────────────────────────
    competitors <- comp$competitors
    
    get_side <- function(side) {
      if (is.null(competitors) || length(competitors) == 0) return(NULL)
      idx <- which(sapply(competitors, function(x) safe_chr(x$homeAway) == side))
      if (length(idx) == 0) return(NULL)
      competitors[[idx[1]]]
    }
    
    extract_team <- function(cx) {
      na_team <- list(
        id = NA_integer_, uid = NA_character_,
        location = NA_character_, name = NA_character_,
        abbreviation = NA_character_, display_name = NA_character_,
        short_display_name = NA_character_,
        color = NA_character_, alternate_color = NA_character_,
        is_active = NA, venue_id = NA_integer_,
        logo = NA_character_, conference_id = NA_integer_,
        score = NA_integer_, winner = NA,
        current_rank = 99L,
        linescores = NA_character_, records = NA_character_
      )
      if (is.null(cx)) return(na_team)
      
      team <- cx$team
      
      linescores_str <- tryCatch(
        as.character(jsonlite::toJSON(safe_vec(cx$linescores, list()), auto_unbox = FALSE)),
        error = function(e) NA_character_
      )
      records_str <- tryCatch(
        as.character(jsonlite::toJSON(safe_vec(cx$records, list()), auto_unbox = FALSE)),
        error = function(e) NA_character_
      )
      
      # Current rank: ESPN uses curatedRank; hoopR stores 99 for unranked
      cur_rank <- tryCatch({
        r <- safe_int(cx$curatedRank$current)
        if (is.na(r)) 99L else as.integer(r)
      }, error = function(e) 99L)
      
      list(
        id                 = safe_int(team$id),
        uid                = safe_chr(team$uid),
        location           = safe_chr(team$location),         # e.g. "Duke"
        name               = safe_chr(team$name),             # e.g. "Blue Devils"
        abbreviation       = safe_chr(team$abbreviation),
        display_name       = safe_chr(team$displayName),      # "Duke Blue Devils"
        short_display_name = safe_chr(team$shortDisplayName), # "Duke"
        color              = safe_chr(team$color),
        alternate_color    = safe_chr(team$alternateColor),
        is_active          = safe_lgl(team$isActive),
        venue_id           = safe_int(team$venue$id),
        logo               = tryCatch(safe_chr(team$logos[[1]]$href),
                                      error = function(e) NA_character_),
        conference_id      = safe_int(team$conferenceId),
        score              = safe_int(cx$score),
        winner             = safe_lgl(cx$winner),
        current_rank       = cur_rank,
        linescores         = linescores_str,
        records            = records_str
      )
    }
    
    h <- extract_team(get_side("home"))
    a <- extract_team(get_side("away"))
    
    # ── Availability flags ────────────────────────────────────────────────────
    # hoopR sets game_json / PBP / team_box / player_box TRUE for completed games
    is_final   <- isTRUE(status_type_completed)
    game_json     <- is_final
    game_json_url <- if (is_final) glue("{WEHOOP_RAW_BASE}/{game_id}.json") else NA_character_
    PBP        <- is_final && isTRUE(play_by_play_avail)
    team_box   <- is_final
    player_box <- is_final
    
    # ── Assemble row — column order matches wehoop::load_wbb_schedule() ────────
    tibble(
      id                        = game_id,
      uid                       = uid,
      date                      = date_raw,
      attendance                = attendance,
      time_valid                = time_valid,
      neutral_site              = neutral_site,
      conference_competition    = conference_comp,
      play_by_play_available    = play_by_play_avail,
      recent                    = recent,
      start_date                = start_date,
      broadcast                 = broadcast,
      highlights                = highlights,
      notes_type                = notes_type,
      notes_headline            = notes_headline,
      broadcast_market          = broadcast_market,
      broadcast_name            = broadcast_name,
      type_id                   = type_id,
      type_abbreviation         = type_abbreviation,
      venue_id                  = venue_id,
      venue_full_name           = venue_full_name,
      venue_address_city        = venue_address_city,
      venue_address_state       = venue_address_state,
      venue_indoor              = venue_indoor,
      status_clock              = status_clock,
      status_display_clock      = status_display_clock,
      status_period             = status_period,
      status_type_id            = status_type_id,
      status_type_name          = status_type_name,
      status_type_state         = status_type_state,
      status_type_completed     = status_type_completed,
      status_type_description   = status_type_description,
      status_type_detail        = status_type_detail,
      status_type_short_detail  = status_type_short_detail,
      format_regulation_periods = format_regulation_periods,
      # Home team — hoopR uses home_* not home_team_*
      home_id                   = h$id,
      home_uid                  = h$uid,
      home_location             = h$location,
      home_name                 = h$name,
      home_abbreviation         = h$abbreviation,
      home_display_name         = h$display_name,
      home_short_display_name   = h$short_display_name,
      home_color                = h$color,
      home_alternate_color      = h$alternate_color,
      home_is_active            = h$is_active,
      home_venue_id             = h$venue_id,
      home_logo                 = h$logo,
      home_conference_id        = h$conference_id,
      home_score                = h$score,
      home_winner               = h$winner,
      home_current_rank         = h$current_rank,
      home_linescores           = h$linescores,
      home_records              = h$records,
      # Away team
      away_id                   = a$id,
      away_uid                  = a$uid,
      away_location             = a$location,
      away_name                 = a$name,
      away_abbreviation         = a$abbreviation,
      away_display_name         = a$display_name,
      away_short_display_name   = a$short_display_name,
      away_color                = a$color,
      away_alternate_color      = a$alternate_color,
      away_is_active            = a$is_active,
      away_venue_id             = a$venue_id,
      away_logo                 = a$logo,
      away_conference_id        = a$conference_id,
      away_score                = a$score,
      away_winner               = a$winner,
      away_current_rank         = a$current_rank,
      away_linescores           = a$linescores,
      away_records              = a$records,
      # Trailing identifiers (hoopR puts these after team columns)
      game_id                   = game_id,
      season                    = season,
      season_type               = season_type,
      status_type_alt_detail    = status_type_alt_detail,
      tournament_id             = tournament_id,
      groups_id                 = groups_id,
      groups_name               = groups_name,
      groups_short_name         = groups_short_name,
      groups_is_conference      = groups_is_conference,
      game_json                 = game_json,
      game_json_url             = game_json_url,
      game_date_time            = game_date_time,
      game_date                 = game_date,
      PBP                       = PBP,
      team_box                  = team_box,
      player_box                = player_box
    )
  })
}


# =============================================================================
# FETCH: one date, one or more ESPN group IDs, deduplicated
# =============================================================================

fetch_wbb_scoreboard_date <- function(date_str, groups = c(50, 100)) {
  all_rows <- map_dfr(groups, function(g) {
    url <- glue("{ESPN_WBB_SCOREBOARD}?groups={g}&limit=500&dates={date_str}")
    tryCatch({
      res <- httr::GET(url, httr::timeout(20))
      if (httr::status_code(res) != 200) {
        message(glue("  [WARN] HTTP {httr::status_code(res)} group={g} date={date_str}"))
        return(tibble())
      }
      json <- jsonlite::fromJSON(
        httr::content(res, as = "text", encoding = "UTF-8"),
        simplifyVector = TRUE, simplifyDataFrame = FALSE
      )
      parse_wbb_scoreboard_json(json)
    }, error = function(e) {
      message(glue("  [ERROR] group={g} date={date_str}: {conditionMessage(e)}"))
      tibble()
    })
  })
  
  if (nrow(all_rows) > 0 && "game_id" %in% names(all_rows))
    all_rows <- distinct(all_rows, game_id, .keep_all = TRUE)
  
  all_rows
}


# =============================================================================
# MAIN: load_wbb_schedule_espn()
# =============================================================================

#' Load WBB schedule directly from ESPN API, matching wehoop::load_wbb_schedule()
#'
#' @param season      Integer season year (e.g. 2026)
#' @param start_date  "YYYY-MM-DD". Defaults to Nov 1 of prior year.
#' @param end_date    "YYYY-MM-DD". Defaults to today or Apr 10, whichever first.
#' @param groups      ESPN group IDs. 50 = all D1, 100 = NCAA Tournament.
#' @param delay_sec   Pause between requests (seconds).
#' @param verbose     Print progress messages.
#'
#' @return tibble with same 86-column schema as wehoop::load_wbb_schedule()
load_wbb_schedule_espn <- function(
    season     = 2026,
    start_date = NULL,
    end_date   = NULL,
    groups     = c(50, 100),
    delay_sec  = 0.3,
    verbose    = TRUE
) {
  start_date <- if (is.null(start_date)) as.Date(paste0(season - 1, "-11-01")) else as.Date(start_date)
  end_date   <- if (is.null(end_date))   min(Sys.Date(), as.Date(paste0(season, "-04-15"))) else as.Date(end_date)
  
  dates        <- seq(start_date, end_date, by = "day")
  date_strings <- format(dates, "%Y%m%d")
  
  if (verbose) message(glue(
    "load_wbb_schedule_espn(): {length(date_strings)} dates ",
    "({start_date} to {end_date}), groups=[{paste(groups, collapse=',')}]"
  ))
  
  all_rows <- map_dfr(seq_along(date_strings), function(i) {
    if (verbose && (i == 1 || i %% 30 == 0 || i == length(date_strings)))
      message(glue("  {i}/{length(date_strings)}: {date_strings[i]}"))
    if (i > 1) Sys.sleep(delay_sec)
    fetch_wbb_scoreboard_date(date_strings[i], groups = groups)
  })
  
  if (nrow(all_rows) == 0) {
    warning("load_wbb_schedule_espn(): no games returned.")
    return(tibble())
  }
  
  all_rows <- all_rows %>%
    distinct(game_id, .keep_all = TRUE) %>%
    arrange(game_date, game_id)
  
  if (verbose) message(glue(
    "Done: {n_distinct(all_rows$game_id)} games, ",
    "{min(all_rows$game_date, na.rm=TRUE)} to {max(all_rows$game_date, na.rm=TRUE)}"
  ))
  
  all_rows
}


# =============================================================================
# MERGE: simple bind_rows with game_id deduplication
# New rows replace existing rows for the same game_id (picks up status updates)
# =============================================================================

#' Combine wehoop schedule baseline with fresh ESPN pull
#'
#' Uses bind_rows so column mismatches fill with NA rather than erroring.
#' For game_ids present in both, the new_rows version wins (fresher status/score).
merge_wbb_schedule <- function(existing, new_rows) {
  if (is.null(existing) || nrow(existing) == 0) return(new_rows)
  if (is.null(new_rows)  || nrow(new_rows)  == 0) return(existing)
  
  combined <- existing %>%
    filter(!game_id %in% new_rows$game_id) %>%
    bind_rows(new_rows) %>%
    arrange(game_date, game_id)
  
  message(glue(
    "merge_wbb_schedule(): {nrow(existing)} existing + ",
    "{nrow(new_rows)} new/updated = {nrow(combined)} total"
  ))
  combined
}


# =============================================================================
# DIAGNOSTIC
# =============================================================================

diag_wbb_scoreboard <- function(date_str = format(Sys.Date() - 1, "%Y%m%d"), group = 50) {
  url <- glue("{ESPN_WBB_SCOREBOARD}?groups={group}&limit=500&dates={date_str}")
  message("Fetching: ", url)
  res  <- httr::GET(url, httr::timeout(20))
  message("HTTP status: ", httr::status_code(res))
  json <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"),
                             simplifyVector = TRUE, simplifyDataFrame = FALSE)
  n <- length(json$events)
  message("Events: ", n)
  if (n > 0) {
    ev <- json$events[[1]]
    message("First: id=", ev$id, "  date=", ev$date, "  name=", ev$name)
  }
  invisible(json)
}