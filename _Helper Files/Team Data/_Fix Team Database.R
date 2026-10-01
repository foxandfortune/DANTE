library(tidyverse)

# Fixes team_database.rds and team_database_wbb.rds after the 2026-27 247 team update:
#   1. Fills in Team where it's NA (9 schools that only had display_name set)
#   2. Adds New Haven and West Florida
# Also includes a helper for finding Action Network IDs for teams that aren't
# mapped yet (see the bottom of the script).
#
# Safe to re-run: Team is only filled where it's NA, and teams are only added
# if their ESPN team_id isn't already in the file.

# Set working directory -------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
setwd('..')

save_changes <- TRUE   # FALSE = dry run, just print what would change

# New teams --------------------------------------------------------------------
## ESPN IDs from site.api.espn.com; Action Network IDs from the AN scoreboard API.
## West Florida wasn't D1 last season, so it has no AN ID yet. Use
## find_unmapped_an_teams() below once games start and add it then.
## odds_api is left NA (same as West Georgia / Mercyhurst) until you know the
## exact name The Odds API uses.
new_teams_base <- tribble(
  ~Team,                    ~team_id, ~short_name,    ~mascot,     ~nickname,      ~team,          ~display_name,
  "New Haven Chargers",     "2441",   "New Haven",    "Chargers",  "New Haven",    "New Haven",    "New Haven Chargers",
  "West Florida Argonauts", "2697",   "West Florida", "Argonauts", "West Florida", "West Florida", "West Florida Argonauts"
)

new_teams <- list(
  mbb = new_teams_base %>%
    mutate(action_network    = c("New Haven", NA),
           action_network_id = c(1601, NA),          # numeric, matches team_database.rds
           odds_api          = NA_character_),
  wbb = new_teams_base %>%
    mutate(action_network    = c("New Haven (W)", NA),
           action_network_id = c(6002L, NA_integer_), # integer, matches team_database_wbb.rds
           odds_api          = NA_character_)
)

db_paths <- c(
  mbb = "_Helper Files/Team Data/team_database.rds",
  wbb = "_Helper Files/Team Data/team_database_wbb.rds"
)

# Fix function -----------------------------------------------------------------
fix_team_db <- function(path, additions) {
  db <- readRDS(path)

  ## 1. Fill missing Team keys from display_name
  na_team <- is.na(db$Team) & !is.na(db$display_name)
  filled  <- db$display_name[na_team]
  db$Team[na_team] <- db$display_name[na_team]

  ## 2. Add new teams that aren't already in the file (matched on ESPN team_id)
  to_add <- additions %>%
    filter(!team_id %in% db$team_id) %>%
    select(all_of(names(db)))

  clash <- intersect(to_add$Team, db$Team)
  if (length(clash) > 0) {
    stop(glue::glue("{path}: Team name(s) already used by another team_id: {paste(clash, collapse = ', ')}"))
  }

  db_new <- bind_rows(db, to_add) %>% as.data.frame()

  ## Checks
  stopifnot(
    !anyNA(db_new$Team),
    !anyDuplicated(db_new$Team),
    !anyDuplicated(db_new$team_id),
    identical(sapply(db, class), sapply(db_new, class))
  )

  ## Report
  cat(glue::glue("\n==== {path} ===="), "\n")
  cat(glue::glue("Rows: {nrow(db)} -> {nrow(db_new)} | Team filled: {length(filled)} | Added: {nrow(to_add)}"), "\n")
  if (length(filled) > 0) cat("Team filled in:\n", paste0("  ", filled, "\n"), sep = "")
  if (nrow(to_add) > 0)   cat("Added:\n", paste0("  ", to_add$Team, "\n"), sep = "")

  missing_an <- db_new %>% filter(is.na(action_network_id)) %>% pull(Team)
  if (length(missing_an) > 0) cat("No Action Network ID (odds won't join):\n", paste0("  ", missing_an, "\n"), sep = "")

  if (save_changes) {
    saveRDS(db_new, path)
    message(glue::glue("Saved {path}"))
  }

  invisible(db_new)
}

team_db_mbb <- fix_team_db(db_paths[["mbb"]], new_teams$mbb)
team_db_wbb <- fix_team_db(db_paths[["wbb"]], new_teams$wbb)

if (!save_changes) message("\nDry run - nothing saved (set save_changes <- TRUE to write the files)")


# Action Network ID helper -----------------------------------------------------
## Pulls the AN scoreboard for a set of dates and returns every D1 team AN listed
## whose ID isn't in the team database yet. Run it over a week or two of games
## (e.g. once the 2026-27 season starts) to pick up West Florida or any other
## new school, then add the action_network / action_network_id values above.
##
## Example:
##   find_unmapped_an_teams(seq(as.Date("2026-11-03"), as.Date("2026-11-16"), by = "day"),
##                          league = "ncaab", team_db = team_db_mbb)
##   find_unmapped_an_teams(seq(as.Date("2026-11-03"), as.Date("2026-11-16"), by = "day"),
##                          league = "ncaaw", team_db = team_db_wbb)

an_teams_on_date <- function(date, league = c("ncaab", "ncaaw")) {
  league <- match.arg(league)
  url <- glue::glue("https://api.actionnetwork.com/web/v1/scoreboard/{league}?division=D1&date={format(as.Date(date), '%Y%m%d')}")

  data <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  if (is.null(data) || length(data$games) == 0) return(tibble())

  map_dfr(data$games$teams, ~ tibble(
    action_network_id = .x$id,
    an_full_name      = .x$full_name,
    an_display_name   = .x$display_name
  ))
}

find_unmapped_an_teams <- function(dates, league = c("ncaab", "ncaaw"), team_db) {
  league <- match.arg(league)

  an_teams <- map_dfr(dates, function(d) {
    Sys.sleep(1)
    an_teams_on_date(d, league)
  }) %>%
    distinct(action_network_id, .keep_all = TRUE)

  an_teams %>%
    filter(!action_network_id %in% team_db$action_network_id) %>%
    arrange(an_full_name)
}
