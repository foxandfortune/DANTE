# =============================================================================
# load_wbb_player_box_espn.R
#
# Direct ESPN API replacement for wehoop::load_wbb_player_box(seasons = 2026).
#
# Same endpoint as team box and PBP scripts:
#   https://site.web.api.espn.com/apis/site/v2/sports/basketball/
#     mens-college-basketball/summary?event={game_id}
# Parses json$boxscore$players into the 54-column wehoop player box schema.
#
# USAGE:
#   source("load_wbb_player_box_espn.R")
#
#   player_box_base <- wehoop::load_wbb_player_box(seasons = 2026) %>%
#     filter(game_date <= as.Date("2026-03-06"))
#
#   player_box_new <- load_wbb_player_box_espn(
#     schedule   = wbb_schedule_full,
#     start_date = "2026-03-07"
#   )
#
#   wbb_player_box_full <- bind_rows(player_box_base, player_box_new)
#   saveRDS(mbb_player_box_full, 'updated_player_box_2026.rds')
# =============================================================================

library(dplyr)
library(purrr)
library(httr)
library(jsonlite)
library(lubridate)
library(glue)

ESPN_WBB_SUMMARY_URL <- "https://site.web.api.espn.com/apis/site/v2/sports/basketball/womens-college-basketball/summary"

if (!exists("safe")) {
  safe     <- function(x, default = NA) { if (is.null(x) || length(x) == 0) default else x[[1]] }
  safe_int <- function(x) as.integer(safe(x, NA_integer_))
  safe_chr <- function(x) as.character(safe(x, NA_character_))
  safe_lgl <- function(x) as.logical(safe(x, NA))
}
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
}

# =============================================================================
# HELPERS
# =============================================================================

# Parse minutes string "MM:SS" or "MM" to decimal minutes
parse_minutes <- function(s) {
  if (is.na(s) || s == "" || s == "--") return(NA_real_)
  parts <- strsplit(s, ":")[[1]]
  if (length(parts) == 2) {
    as.numeric(parts[1]) + as.numeric(parts[2]) / 60
  } else {
    suppressWarnings(as.numeric(parts[1]))
  }
}

# Split "made-attempted" string into a named list
split_ma <- function(s) {
  if (is.na(s) || s == "--") return(list(made = NA_integer_, att = NA_integer_))
  parts <- strsplit(s, "-")[[1]]
  list(
    made = suppressWarnings(as.integer(parts[1])),
    att  = suppressWarnings(as.integer(parts[2]))
  )
}

# Safe integer from a plain display string ("5", "--", NA)
parse_int <- function(s) {
  if (is.na(s) || s == "--" || s == "") return(NA_integer_)
  suppressWarnings(as.integer(s))
}

# Build team slug
make_slug <- function(location, name) {
  if (is.na(location) || is.na(name)) return(NA_character_)
  slug <- tolower(paste(location, name))
  slug <- gsub("[^a-z0-9]+", "-", slug)
  gsub("^-|-$", "", slug)
}


# =============================================================================
# CORE PARSER
# =============================================================================

#' Parse ESPN summary JSON into wehoop-schema player box tibble
#'
#' @param json    Parsed JSON from the ESPN summary endpoint
#' @param game_id Integer game ID (for error messages)
#' @return tibble with one row per player, 54 columns
parse_wbb_player_box_json <- function(json, game_id) {
  
  # ── Game metadata ──────────────────────────────────────────────────────────
  header <- json$header
  comp   <- if (!is.null(header)) header$competitions[[1]] else NULL
  
  date_raw       <- if (!is.null(comp)) safe_chr(comp$date) else NA_character_
  game_date      <- tryCatch(as.Date(substr(date_raw, 1, 10)), error = function(e) as.Date(NA))
  game_date_time <- tryCatch(
    lubridate::ymd_hms(date_raw, quiet = TRUE) %>% lubridate::with_tz("America/New_York"),
    error = function(e) as.POSIXct(NA)
  )
  season      <- safe_int(header$season$year)
  season_type <- safe_int(header$season$type)
  
  # ── Competitor info (score, winner, home/away) ────────────────────────────
  competitors <- if (!is.null(comp)) comp$competitors else list()
  
  get_comp_by_id <- function(team_id_str) {
    idx <- which(sapply(competitors, function(x)
      as.character(safe_chr(x$team$id)) == as.character(team_id_str)))
    if (length(idx) == 0) return(NULL)
    competitors[[idx[1]]]
  }
  
  # ── Player data from boxscore$players ─────────────────────────────────────
  players_data <- json$boxscore$players
  if (is.null(players_data) || length(players_data) == 0) {
    message(glue("  [WARN] game {game_id}: no boxscore$players in JSON"))
    return(tibble())
  }
  
  # Each element of players_data is one team's roster
  team_rows <- map_dfr(seq_along(players_data), function(ti) {
    
    td   <- players_data[[ti]]
    team <- td$team
    
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
    t_logo    <- tryCatch(safe_chr(team$logos[[1]]$href),
                          error = function(e) NA_character_)
    t_slug    <- make_slug(t_loc, t_name)
    
    # Team game context from header
    cx         <- get_comp_by_id(t_id)
    t_home_away<- if (!is.null(cx)) safe_chr(cx$homeAway)  else NA_character_
    t_score    <- if (!is.null(cx)) safe_int(cx$score)     else NA_integer_
    t_winner   <- if (!is.null(cx)) safe_lgl(cx$winner)    else NA
    
    # Opponent team (the other competitor)
    opp_cx <- Filter(function(x)
      as.character(safe_chr(x$team$id)) != as.character(t_id), competitors)
    opp <- if (length(opp_cx) > 0) opp_cx[[1]] else NULL
    opp_team   <- if (!is.null(opp)) opp$team else NULL
    
    opp_id     <- safe_int(opp_team$id)
    opp_name   <- safe_chr(opp_team$name)
    opp_loc    <- safe_chr(opp_team$location)
    opp_disp   <- safe_chr(opp_team$displayName)
    opp_abbrev <- safe_chr(opp_team$abbreviation)
    opp_logo   <- tryCatch(safe_chr(opp_team$logos[[1]]$href),
                           error = function(e) NA_character_)
    opp_color  <- safe_chr(opp_team$color)
    opp_alt    <- safe_chr(opp_team$alternateColor)
    opp_score  <- if (!is.null(opp)) safe_int(opp$score) else NA_integer_
    
    # ── Stat column labels and athlete rows ──────────────────────────────────
    # td$statistics is a list with one element (the stats group for this team)
    # that contains: names (labels), athletes (list of player rows)
    stats_group <- if (!is.null(td$statistics) && length(td$statistics) > 0)
      td$statistics[[1]] else NULL
    
    if (is.null(stats_group)) return(tibble())
    
    # The stat names in order — ESPN uses: min, fg, 3pt, ft, oreb, dreb, reb,
    # ast, stl, blk, to, pf, pts  (plus possibly more)
    stat_names <- safe_chr(stats_group$names %||% list())
    # Actually ESPN provides stat labels via 'names' field as a vector
    # We'll map positionally from the athletes$statistics array
    # Safe fallback: use known fixed order if names unavailable
    KNOWN_ORDER <- c("min", "fg", "3pt", "ft", "oreb", "dreb", "reb",
                     "ast", "stl", "blk", "to", "pf", "pts")
    
    # Use the labels array if present, else fall back to known order
    labels <- if (!is.null(stats_group$names) && length(stats_group$names) > 0)
      unlist(stats_group$names)
    else
      KNOWN_ORDER
    
    athletes <- stats_group$athletes
    if (is.null(athletes) || length(athletes) == 0) return(tibble())
    
    map_dfr(seq_along(athletes), function(ai) {
      a      <- athletes[[ai]]
      ath    <- a$athlete
      
      # Athlete identity
      ath_id     <- safe_int(ath$id)
      ath_disp   <- safe_chr(ath$displayName)
      ath_short  <- safe_chr(ath$shortName)
      ath_jersey <- safe_chr(ath$jersey)
      ath_head   <- tryCatch(safe_chr(ath$headshot$href),
                             error = function(e) NA_character_)
      ath_pos_name  <- tryCatch(safe_chr(ath$position$name),
                                error = function(e) NA_character_)
      ath_pos_abbr  <- tryCatch(safe_chr(ath$position$abbreviation),
                                error = function(e) NA_character_)
      
      # Participation flags
      starter      <- isTRUE(a$starter)
      ejected      <- isTRUE(a$ejected)
      did_not_play <- isTRUE(a$didNotPlay)
      active       <- isTRUE(a$active)
      
      # Stats: positional array of displayValue strings
      raw_stats <- if (!is.null(a$statistics)) unlist(a$statistics) else character(0)
      
      get_stat <- function(label) {
        idx <- which(labels == label)
        if (length(idx) == 0 || idx[1] > length(raw_stats)) return(NA_character_)
        raw_stats[idx[1]]
      }
      
      # Minutes
      minutes_str <- get_stat("min")
      minutes     <- parse_minutes(minutes_str)
      
      # Split stats
      fg   <- split_ma(get_stat("fg"))
      fg3  <- split_ma(get_stat("3pt"))
      ft   <- split_ma(get_stat("ft"))
      
      # Integer stats
      oreb <- parse_int(get_stat("oreb"))
      dreb <- parse_int(get_stat("dreb"))
      reb  <- parse_int(get_stat("reb"))
      ast  <- parse_int(get_stat("ast"))
      stl  <- parse_int(get_stat("stl"))
      blk  <- parse_int(get_stat("blk"))
      to   <- parse_int(get_stat("to"))
      pf   <- parse_int(get_stat("pf"))
      pts  <- parse_int(get_stat("pts"))
      
      tibble(
        game_id                           = as.integer(game_id),
        season                            = season,
        season_type                       = season_type,
        game_date                         = game_date,
        game_date_time                    = game_date_time,
        athlete_id                        = ath_id,
        athlete_display_name              = ath_disp,
        team_id                           = t_id,
        team_name                         = t_name,
        team_location                     = t_loc,
        team_short_display_name           = t_short,
        minutes                           = minutes,
        field_goals_made                  = fg$made,
        field_goals_attempted             = fg$att,
        three_point_field_goals_made      = fg3$made,
        three_point_field_goals_attempted = fg3$att,
        free_throws_made                  = ft$made,
        free_throws_attempted             = ft$att,
        offensive_rebounds                = oreb,
        defensive_rebounds                = dreb,
        rebounds                          = reb,
        assists                           = ast,
        steals                            = stl,
        blocks                            = blk,
        turnovers                         = to,
        fouls                             = pf,
        points                            = pts,
        starter                           = starter,
        ejected                           = ejected,
        did_not_play                      = did_not_play,
        active                            = active,
        athlete_jersey                    = ath_jersey,
        athlete_short_name                = ath_short,
        athlete_headshot_href             = ath_head,
        athlete_position_name             = ath_pos_name,
        athlete_position_abbreviation     = ath_pos_abbr,
        team_display_name                 = t_disp,
        team_uid                          = t_uid,
        team_slug                         = t_slug,
        team_logo                         = t_logo,
        team_abbreviation                 = t_abbrev,
        team_color                        = t_color,
        team_alternate_color              = t_alt,
        home_away                         = t_home_away,
        team_winner                       = t_winner,
        team_score                        = t_score,
        opponent_team_id                  = opp_id,
        opponent_team_name                = opp_name,
        opponent_team_location            = opp_loc,
        opponent_team_display_name        = opp_disp,
        opponent_team_abbreviation        = opp_abbrev,
        opponent_team_logo                = opp_logo,
        opponent_team_color               = opp_color,
        opponent_team_alternate_color     = opp_alt,
        opponent_team_score               = opp_score
      )
    })
  })
  
  team_rows
}


# =============================================================================
# FETCH: single game
# =============================================================================

fetch_wbb_player_box_game <- function(game_id) {
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
    parse_wbb_player_box_json(json, game_id)
  }, error = function(e) {
    message(glue("  [ERROR] game {game_id}: {conditionMessage(e)}"))
    tibble()
  })
}


# =============================================================================
# MAIN: load_wbb_player_box_espn()
# =============================================================================

#' Load WBB player box scores directly from ESPN API
#'
#' @param game_ids   Integer vector of game IDs. If NULL, resolved from schedule.
#' @param schedule   tibble from load_wbb_schedule_espn() or wehoop::load_wbb_schedule().
#' @param start_date "YYYY-MM-DD" filter on schedule.
#' @param end_date   "YYYY-MM-DD" filter on schedule. Defaults to today.
#' @param delay_sec  Pause between requests. Default 0.5s.
#' @param verbose    Print progress.
#'
#' @return tibble with same 54-column schema as wehoop::load_wbb_player_box()
load_wbb_player_box_espn <- function(
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
      warning("load_wbb_player_box_espn(): no STATUS_FINAL games in schedule for given date range.")
      return(tibble())
    }
  }
  
  game_ids <- as.integer(unique(game_ids))
  n        <- length(game_ids)
  
  if (verbose) message(glue("load_wbb_player_box_espn(): fetching {n} games..."))
  
  all_rows <- map_dfr(seq_along(game_ids), function(i) {
    gid <- game_ids[[i]]
    if (verbose && (i == 1 || i %% 50 == 0 || i == n))
      message(glue("  {i}/{n}: game_id={gid}"))
    if (i > 1) Sys.sleep(delay_sec)
    fetch_wbb_player_box_game(gid)
  })
  
  if (nrow(all_rows) == 0) {
    warning("load_wbb_player_box_espn(): no rows returned.")
    return(tibble())
  }
  
  all_rows <- all_rows %>%
    arrange(game_date, game_id, team_id, desc(starter), desc(minutes))
  
  if (verbose) message(glue(
    "Done: {n_distinct(all_rows$game_id)} games, {nrow(all_rows)} player rows, ",
    "{min(all_rows$game_date, na.rm=TRUE)} to {max(all_rows$game_date, na.rm=TRUE)}"
  ))
  
  all_rows
}


# =============================================================================
# MERGE
# =============================================================================

#' Merge existing wehoop player box with new ESPN rows
#' New game_ids fully replace existing (all players for that game replaced).
merge_wbb_player_box <- function(existing, new_rows) {
  if (is.null(existing) || nrow(existing) == 0) return(new_rows)
  if (is.null(new_rows)  || nrow(new_rows)  == 0) return(existing)
  
  combined <- existing %>%
    filter(!game_id %in% new_rows$game_id) %>%
    bind_rows(new_rows) %>%
    arrange(game_date, game_id, team_id, desc(starter))
  
  message(glue(
    "merge_wbb_player_box(): {n_distinct(existing$game_id)} existing games + ",
    "{n_distinct(new_rows$game_id)} new games = ",
    "{n_distinct(combined$game_id)} total games, {nrow(combined)} player rows"
  ))
  combined
}