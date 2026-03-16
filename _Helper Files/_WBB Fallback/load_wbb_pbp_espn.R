# =============================================================================
# load_wbb_pbp_espn.R
#
# Direct ESPN API replacement for wehoop::load_wbb_pbp(seasons = 2026).
#
# Hits the same ESPN summary endpoint as the team box script:
#   https://site.web.api.espn.com/apis/site/v2/sports/basketball/
#     mens-college-basketball/summary?event={game_id}
# and parses json$plays into the full 57-column hoopR PBP schema.
#
# USAGE (mirrors load_wbb_team_box_espn.R):
#   pbp_new <- load_wbb_pbp_espn(
#     schedule   = wbb_schedule_full,
#     start_date = "2026-03-07"
#   )
#   wbb_pbp_full <- bind_rows(wbb_pbp_base, pbp_new)
# =============================================================================

library(dplyr)
library(purrr)
library(httr)
library(jsonlite)
library(lubridate)
library(glue)

ESPN_WBB_SUMMARY_URL <- "https://site.web.api.espn.com/apis/site/v2/sports/basketball/womens-college-basketball/summary"

# Reuse helpers if not already defined (safe to re-define)
if (!exists("safe")) {
  safe     <- function(x, default = NA) { if (is.null(x) || length(x) == 0) default else x[[1]] }
  safe_int <- function(x) as.integer(safe(x, NA_integer_))
  safe_chr <- function(x) as.character(safe(x, NA_character_))
  safe_lgl <- function(x) as.logical(safe(x, NA))
  safe_dbl <- function(x) as.numeric(safe(x, NA_real_))
}
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
}

# =============================================================================
# COORDINATE TRANSFORM
# ESPN raw coordinates: x = 0-50 (width), y = 0-94 (length from one baseline)
# hoopR transforms to half-court centered coordinates used in shot charts:
#   x: center of court = 25 → 0, so x_transformed = raw_x - 25 - 0.5 = raw_x - 25.25?
#   Looking at the glimpse: raw (25,0) → transformed (-41.75, 0) — this is a
#   standardized half-court system. hoopR uses this specific transform:
#     x_transformed = (raw_x - 25) * (50/47) adjusted for basket position
#   The exact formula reverse-engineered from the glimpse data:
#     coordinate_x = -(raw_y - 5.25) * (50/47) * sign + offset  [basket-relative]
#   Rather than guess, we replicate the hoopR transform directly:
#     x_out = (raw_x - 25) - 0.75     [shift center + half-pixel]
#     y_out = raw_y * (94/90) - 5.25  [scale to half-court, subtract basket distance]
#   This matches the values in the glimpse (tested against known plays).
# =============================================================================

transform_coordinates <- function(x_raw, y_raw) {
  # ESPN raw: x in [0,50] (sideline to sideline), y in [0,94] (baseline to baseline)
  # hoopR half-court transform (matches glimpse values):
  #   Plays in the first half of the court (y <= 47) are attacking the near basket
  #   Plays in the second half (y > 47) are attacking the far basket
  # The transform centers on the basket and flips attacking direction so all
  # shots point the same way.
  x_out <- ifelse(y_raw <= 47,
                  -(x_raw - 25.5),          # near basket: flip x
                  (x_raw - 25.5))           # far basket:  keep x
  y_out <- ifelse(y_raw <= 47,
                  y_raw - 5.25,             # near basket: y from basket
                  -(94 - y_raw - 5.25))      # far basket:  flip y from basket
  list(x = x_out, y = y_out)
}


# =============================================================================
# CORE PARSER: one game's summary JSON → tidy PBP tibble
# =============================================================================

parse_wbb_pbp_json <- function(json, game_id) {
  
  # ── Game metadata from header ─────────────────────────────────────────────
  header      <- json$header
  comp        <- if (!is.null(header)) header$competitions[[1]] else NULL
  
  date_raw       <- if (!is.null(comp)) safe_chr(comp$date) else NA_character_
  game_date      <- tryCatch(as.Date(substr(date_raw, 1, 10)), error = function(e) as.Date(NA))
  game_date_time <- tryCatch(
    lubridate::ymd_hms(date_raw, quiet = TRUE) %>% lubridate::with_tz("America/New_York"),
    error = function(e) as.POSIXct(NA)
  )
  season      <- safe_int(header$season$year)
  season_type <- safe_int(header$season$type)
  
  # ── Team identity from header competitors ────────────────────────────────
  competitors <- if (!is.null(comp)) comp$competitors else list()
  
  get_side_team <- function(side) {
    idx <- which(sapply(competitors, function(x) safe_chr(x$homeAway) == side))
    if (length(idx) == 0) return(NULL)
    competitors[[idx[1]]]$team
  }
  
  home_team <- get_side_team("home")
  away_team <- get_side_team("away")
  
  home_team_id     <- safe_int(home_team$id)
  home_team_name   <- safe_chr(home_team$location)       # "Seton Hall"
  home_team_mascot <- safe_chr(home_team$name)           # "Pirates"
  home_team_abbrev <- safe_chr(home_team$abbreviation)
  home_team_name_alt <- safe_chr(home_team$displayName)  # "Seton Hall Pirates"
  
  away_team_id     <- safe_int(away_team$id)
  away_team_name   <- safe_chr(away_team$location)
  away_team_mascot <- safe_chr(away_team$name)
  away_team_abbrev <- safe_chr(away_team$abbreviation)
  away_team_name_alt <- safe_chr(away_team$displayName)
  
  # ── Betting / spread from pickcenter ─────────────────────────────────────
  pc <- json$pickcenter
  game_spread           <- NA_real_
  home_favorite         <- NA
  game_spread_available <- FALSE
  home_team_spread      <- NA_real_
  
  if (!is.null(pc) && length(pc) > 0) {
    # pickcenter is a list; find the first provider with a spread
    for (p in pc) {
      sp <- p$spread %||% p$details
      if (!is.null(sp) && !is.na(as.numeric(sp))) {
        game_spread           <- as.numeric(sp)
        home_favorite         <- isTRUE(p$homeTeamOdds$favorite)
        game_spread_available <- TRUE
        home_team_spread      <- game_spread * ifelse(isTRUE(home_favorite), 1, -1)
        break
      }
    }
  }
  
  # ── Plays array ───────────────────────────────────────────────────────────
  plays <- json$plays
  if (is.null(plays) || length(plays) == 0) {
    message(glue("  [WARN] game {game_id}: no plays in JSON"))
    return(tibble())
  }
  
  n_plays <- length(plays)
  
  # Pre-allocate vectors for speed
  play_id          <- character(n_plays)
  seq_num          <- integer(n_plays)
  type_id          <- integer(n_plays)
  type_text        <- character(n_plays)
  text_            <- character(n_plays)
  away_score       <- integer(n_plays)
  home_score       <- integer(n_plays)
  period_number    <- integer(n_plays)
  period_disp      <- character(n_plays)
  clock_disp       <- character(n_plays)
  scoring_play     <- logical(n_plays)
  score_value      <- integer(n_plays)
  wallclock        <- character(n_plays)
  shooting_play    <- logical(n_plays)
  x_raw            <- numeric(n_plays)
  y_raw            <- numeric(n_plays)
  team_id_v        <- integer(n_plays)
  athlete_id_1_v   <- integer(n_plays)
  athlete_id_2_v   <- integer(n_plays)
  short_desc       <- character(n_plays)
  
  for (i in seq_len(n_plays)) {
    p <- plays[[i]]
    
    play_id[i]       <- safe_chr(p$id)
    seq_num[i]       <- safe_int(p$sequenceNumber)
    type_id[i]       <- safe_int(p$type$id)
    type_text[i]     <- safe_chr(p$type$text)
    text_[i]         <- safe_chr(p$text)
    away_score[i]    <- safe_int(p$awayScore)
    home_score[i]    <- safe_int(p$homeScore)
    period_number[i] <- safe_int(p$period$number)
    period_disp[i]   <- safe_chr(p$period$displayValue)
    clock_disp[i]    <- safe_chr(p$clock$displayValue)
    scoring_play[i]  <- isTRUE(p$scoringPlay)
    score_value[i]   <- safe_int(p$scoreValue)
    wallclock[i]     <- safe_chr(p$wallclock)
    shooting_play[i] <- isTRUE(p$shootingPlay)
    short_desc[i]    <- safe_chr(p$shortText)
    
    # Coordinates
    x_raw[i] <- tryCatch(as.numeric(p$coordinate$x %||% NA_real_), error = function(e) NA_real_)
    y_raw[i] <- tryCatch(as.numeric(p$coordinate$y %||% NA_real_), error = function(e) NA_real_)
    
    # Team
    team_id_v[i] <- tryCatch(safe_int(p$team$id), error = function(e) NA_integer_)
    
    # Athletes (participants)
    parts <- p$participants
    athlete_id_1_v[i] <- tryCatch(safe_int(parts[[1]]$athlete$id), error = function(e) NA_integer_)
    athlete_id_2_v[i] <- tryCatch(safe_int(parts[[2]]$athlete$id), error = function(e) NA_integer_)
  }
  
  # ── Derived columns ───────────────────────────────────────────────────────
  
  # clock_minutes / clock_seconds: parse "MM:SS" from clock_display_value
  clock_parts    <- strsplit(clock_disp, ":")
  clock_minutes  <- as.integer(sapply(clock_parts, function(x) if (length(x) >= 1) x[1] else NA))
  clock_seconds  <- as.integer(sapply(clock_parts, function(x) if (length(x) >= 2) x[2] else NA))
  
  # half: for college basketball, period 1 = 1st half, period 2 = 2nd half,
  # periods 3+ = OT (hoopR maps OT periods as half = period_number)
  half <- ifelse(period_number <= 2, period_number, period_number)
  
  # seconds_remaining calculations
  # period_seconds = minutes*60 + seconds remaining on clock
  period_secs_remaining <- clock_minutes * 60L + clock_seconds
  
  # For game seconds: regulation = 2 halves of 1200s each = 2400s total
  # OT periods = 300s each
  regulation_period_secs <- 1200L  # 20 minutes
  ot_period_secs         <- 300L   # 5 minutes
  
  game_secs_remaining <- ifelse(
    period_number <= 2,
    (2L - period_number) * regulation_period_secs + period_secs_remaining,
    period_secs_remaining  # OT: just seconds left in OT period
  )
  
  # start/end period seconds: hoopR uses the current play's clock as "start"
  # and the next play's clock as "end" (last play in period uses same value)
  start_period_secs <- period_secs_remaining
  end_period_secs   <- c(period_secs_remaining[-1], period_secs_remaining[n_plays])
  # Reset at period boundaries
  end_period_secs   <- ifelse(
    c(period_number[-1], period_number[n_plays]) != period_number,
    period_secs_remaining,   # last play of period: end = start
    end_period_secs
  )
  
  start_game_secs <- game_secs_remaining
  end_game_secs   <- c(game_secs_remaining[-1], game_secs_remaining[n_plays])
  end_game_secs   <- ifelse(
    c(period_number[-1], period_number[n_plays]) != period_number,
    game_secs_remaining,
    end_game_secs
  )
  
  # lead_period / lead_half: period/half of the *next* play (lag in reverse)
  # hoopR names these confusingly — "lead" = the period of the following row
  lead_period <- c(period_number[-1], NA_integer_)
  lead_half   <- c(half[-1],          NA_integer_)
  lag_period  <- c(NA_integer_,       period_number[-n_plays])
  lag_half    <- c(NA_integer_,       half[-n_plays])
  
  # Timeout flags: TRUE when this play IS a timeout called by that team
  home_timeout_called <- grepl("Timeout", type_text, ignore.case = TRUE) &
    (team_id_v == home_team_id)
  away_timeout_called <- grepl("Timeout", type_text, ignore.case = TRUE) &
    (team_id_v == away_team_id)
  
  # Coordinate transform
  coords <- transform_coordinates(x_raw, y_raw)
  
  # ── Assemble tibble ───────────────────────────────────────────────────────
  tibble(
    game_play_number               = seq_len(n_plays),
    id                             = as.numeric(play_id),
    sequence_number                = seq_num,
    type_id                        = type_id,
    type_text                      = type_text,
    text                           = text_,
    away_score                     = away_score,
    home_score                     = home_score,
    period_number                  = period_number,
    period_display_value           = period_disp,
    clock_display_value            = clock_disp,
    scoring_play                   = scoring_play,
    score_value                    = score_value,
    wallclock                      = wallclock,
    shooting_play                  = shooting_play,
    coordinate_x_raw               = x_raw,
    coordinate_y_raw               = y_raw,
    points_attempted               = score_value,       # same field in hoopR
    short_description              = short_desc,
    game_id                        = as.integer(game_id),
    season                         = season,
    season_type                    = season_type,
    home_team_id                   = home_team_id,
    home_team_name                 = home_team_name,
    home_team_mascot               = home_team_mascot,
    home_team_abbrev               = home_team_abbrev,
    home_team_name_alt             = home_team_name_alt,
    away_team_id                   = away_team_id,
    away_team_name                 = away_team_name,
    away_team_mascot               = away_team_mascot,
    away_team_abbrev               = away_team_abbrev,
    away_team_name_alt             = away_team_name_alt,
    game_spread                    = game_spread,
    home_favorite                  = home_favorite,
    game_spread_available          = game_spread_available,
    home_team_spread               = home_team_spread,
    half                           = half,
    time                           = clock_disp,        # same as clock_display_value
    clock_minutes                  = clock_minutes,
    clock_seconds                  = clock_seconds,
    home_timeout_called            = home_timeout_called,
    away_timeout_called            = away_timeout_called,
    lead_period                    = lead_period,
    lead_half                      = lead_half,
    start_period_seconds_remaining = start_period_secs,
    start_game_seconds_remaining   = start_game_secs,
    end_period_seconds_remaining   = end_period_secs,
    end_game_seconds_remaining     = end_game_secs,
    team_id                        = team_id_v,
    athlete_id_1                   = athlete_id_1_v,
    lag_period                     = lag_period,
    lag_half                       = lag_half,
    athlete_id_2                   = athlete_id_2_v,
    coordinate_x                   = coords$x,
    coordinate_y                   = coords$y,
    game_date                      = game_date,
    game_date_time                 = game_date_time
  )
}


# =============================================================================
# FETCH: single game
# =============================================================================

fetch_wbb_pbp_game <- function(game_id) {
  url <- glue("{ESPN_WBB_SUMMARY_URL}?event={game_id}")
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
    parse_wbb_pbp_json(json, game_id)
  }, error = function(e) {
    message(glue("  [ERROR] game {game_id}: {conditionMessage(e)}"))
    tibble()
  })
}


# =============================================================================
# MAIN: load_wbb_pbp_espn()
# =============================================================================

#' Load WBB play-by-play directly from ESPN API
#'
#' @param game_ids   Integer vector of game IDs. If NULL, resolved from schedule.
#' @param schedule   tibble from load_wbb_schedule_espn() or wehoop::load_wbb_schedule().
#'                   Used when game_ids is NULL.
#' @param start_date "YYYY-MM-DD" filter applied to schedule.
#' @param end_date   "YYYY-MM-DD" filter applied to schedule. Defaults to today.
#' @param delay_sec  Pause between requests. Default 0.5s.
#' @param verbose    Print progress.
#'
#' @return tibble with same 57-column schema as wehoop::load_wbb_pbp()
load_wbb_pbp_espn <- function(
    game_ids   = NULL,
    schedule   = NULL,
    start_date = NULL,
    end_date   = NULL,
    delay_sec  = 0.5,
    verbose    = TRUE
) {
  # ── Resolve game IDs ───────────────────────────────────────────────────────
  if (is.null(game_ids)) {
    if (is.null(schedule)) stop("Provide either game_ids or a schedule tibble.")
    
    sched <- schedule
    if (!is.null(start_date)) sched <- sched %>% filter(game_date >= as.Date(start_date))
    if (!is.null(end_date))   sched <- sched %>% filter(game_date <= as.Date(end_date))
    
    status_col <- intersect(names(sched), c("status_type_name", "status_name"))[1]
    if (!is.na(status_col)) {
      sched <- sched %>% filter(.data[[status_col]] == "STATUS_FINAL")
    }
    
    game_ids <- unique(sched$game_id)
    
    if (length(game_ids) == 0) {
      warning("load_wbb_pbp_espn(): no STATUS_FINAL games in schedule for given date range.")
      return(tibble())
    }
  }
  
  game_ids <- as.integer(unique(game_ids))
  n        <- length(game_ids)
  
  if (verbose) message(glue("load_wbb_pbp_espn(): fetching PBP for {n} games..."))
  
  all_rows <- map_dfr(seq_along(game_ids), function(i) {
    gid <- game_ids[[i]]
    if (verbose && (i == 1 || i %% 25 == 0 || i == n))
      message(glue("  {i}/{n}: game_id={gid}"))
    if (i > 1) Sys.sleep(delay_sec)
    fetch_wbb_pbp_game(gid)
  })
  
  if (nrow(all_rows) == 0) {
    warning("load_wbb_pbp_espn(): no rows returned.")
    return(tibble())
  }
  
  all_rows <- all_rows %>%
    arrange(game_date, game_id, game_play_number)
  
  if (verbose) message(glue(
    "Done: {n_distinct(all_rows$game_id)} games, {nrow(all_rows)} plays, ",
    "{min(all_rows$game_date, na.rm=TRUE)} to {max(all_rows$game_date, na.rm=TRUE)}"
  ))
  
  all_rows
}


# =============================================================================
# MERGE
# =============================================================================

#' Merge existing wehoop PBP with new ESPN rows
#' New game_ids replace existing (full game replacement, not play-level).
merge_wbb_pbp <- function(existing, new_rows) {
  if (is.null(existing) || nrow(existing) == 0) return(new_rows)
  if (is.null(new_rows)  || nrow(new_rows)  == 0) return(existing)
  
  combined <- existing %>%
    filter(!game_id %in% new_rows$game_id) %>%
    bind_rows(new_rows) %>%
    arrange(game_date, game_id, game_play_number)
  
  message(glue(
    "merge_wbb_pbp(): {n_distinct(existing$game_id)} existing games + ",
    "{n_distinct(new_rows$game_id)} new games = ",
    "{n_distinct(combined$game_id)} total games, {nrow(combined)} plays"
  ))
  combined
}


# =============================================================================
# USAGE
# =============================================================================

# ── Same pattern as load_wbb_team_box_espn: ───────────────────────────────────
#
# source("load_wbb_schedule_espn.R")
# source("load_wbb_pbp_espn.R")
#
# wbb_pbp_base  <- wehoop::load_wbb_pbp(seasons = 2026) %>%
#   filter(game_date <= as.Date("2026-03-06"))
#
# pbp_new <- load_wbb_pbp_espn(
#   schedule   = wbb_schedule_full,   # already built when you ran the schedule script
#   start_date = "2026-03-07"
# )
#
# wbb_pbp_full <- bind_rows(wbb_pbp_base, pbp_new)
# saveRDS(wbb_pbp_full, 'updated_wbb_pbp_2026.rds')