library(rvest)
library(tidyverse)

# Pulls the current team list from 247Sports' college basketball team index and
# adds any teams missing from _Helper Files/Team Data/teams247.rds.
#
# teams247.rds keeps two columns:
#   Team - the team key used across DANTE (matches team_database.rds$Team)
#   URL  - the 247 college page (https://247sports.com/college/<slug>/), or
#          https://247sports.com/ when 247 has no team page for that school
#
# Matching is done by 247 URL first, then by team name (manual fixes, then
# team_database names/aliases), so 247 renames (e.g. "UConn Huskies") map back
# to the existing DANTE key (e.g. "Connecticut Huskies") instead of creating
# duplicates. Existing rows are never removed.

# Set working directory -------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
setwd('..')
setwd('..')
setwd('..')

# Settings ---------------------------------------------------------------------
teams_url       <- "https://247sports.com/League/NCAA-BK/Teams/"
teams247_path   <- "_Helper Files/Team Data/teams247.rds"
team_db_path    <- "_Helper Files/Team Data/team_database.rds"
placeholder_url <- "https://247sports.com/"

update_urls  <- TRUE   # also refresh URLs for teams already in the file (placeholder or changed slug)
save_changes <- FALSE   # FALSE = dry run, just print what would change

## Manual name fixes: 247 name -> DANTE team key (team_database.rds$Team, or
## display_name where Team is NA). Add to this when the report flags a team.
name_fixes <- c(
  "Queens Royals"         = "Queens University Royals",
  "East Texas A&M Lions"  = "Texas A&M-Commerce Lions",
  "St. Thomas Tommies"    = "St. Thomas - Minnesota Tommies",
  "Tarleton State Texans" = "Tarleton Texans"
)

## Schools listed on the 247 page that aren't D1 and shouldn't be added
exclude_teams <- c("LeMoyne-Owen Magicians")

# Helpers ----------------------------------------------------------------------
normalize_name <- function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("\\bsaint\\b", "st") %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

clean_url <- function(href) {
  url <- xml2::url_absolute(href, teams_url)
  url <- str_replace(url, "^http://", "https://")
  if_else(str_ends(url, "/"), url, paste0(url, "/"))
}

# Scrape 247 team index --------------------------------------------------------
page <- read_html(teams_url)

scraped <- page %>%
  html_elements("div.conference-section") %>%
  map_dfr(function(section) {
    links <- html_elements(section, "li.team-card > a.pro-team")
    tibble(
      conference = section %>% html_element(".conference-title") %>% html_text2() %>% str_squish(),
      team_247   = links %>% html_text2() %>% str_squish(),
      URL        = links %>% html_attr("href") %>% clean_url()
    )
  }) %>%
  filter(team_247 != "", !team_247 %in% exclude_teams) %>%
  distinct(team_247, .keep_all = TRUE)

if (nrow(scraped) < 300) {
  stop(glue::glue("Only {nrow(scraped)} teams scraped - the 247 page layout has probably changed."))
}

message(glue::glue("Scraped {nrow(scraped)} teams across {n_distinct(scraped$conference)} conferences"))

# Load existing data -----------------------------------------------------------
teams247_old <- readRDS(teams247_path) %>%
  as_tibble() %>%
  mutate(across(c(Team, URL), as.character))

team_db <- readRDS(team_db_path) %>% as_tibble()

## Name lookup: any known name for a school -> DANTE team key
name_lookup <- team_db %>%
  transmute(
    key = coalesce(Team, display_name),
    a1  = Team,
    a2  = display_name,
    a3  = str_c(team, " ", mascot),
    a4  = str_c(short_name, " ", mascot)
  ) %>%
  filter(!is.na(key)) %>%
  pivot_longer(a1:a4, values_to = "alias") %>%
  select(key, alias) %>%
  bind_rows(transmute(teams247_old, key = Team, alias = Team)) %>%
  filter(!is.na(alias)) %>%
  mutate(norm = normalize_name(alias)) %>%
  distinct(norm, key) %>%
  group_by(norm) %>%
  filter(n_distinct(key) == 1) %>%   # drop aliases that point at more than one school
  ungroup()

# Match scraped teams to DANTE keys --------------------------------------------
url_lookup <- teams247_old %>%
  filter(URL != placeholder_url) %>%
  distinct(URL, .keep_all = TRUE) %>%
  select(URL, team_by_url = Team)

matched <- scraped %>%
  mutate(norm = normalize_name(team_247)) %>%
  left_join(url_lookup, by = "URL") %>%
  left_join(select(name_lookup, norm, team_by_name = key), by = "norm") %>%
  mutate(
    team_by_fix = unname(name_fixes[team_247]),
    Team = coalesce(team_by_url, team_by_fix, team_by_name, team_247),
    match_type = case_when(
      !is.na(team_by_url)  ~ "url",
      !is.na(team_by_fix)  ~ "manual fix",
      !is.na(team_by_name) ~ "name",
      TRUE                 ~ "unmatched (247 name used)"
    ),
    in_file = Team %in% teams247_old$Team
  )

## New teams to add
new_teams <- matched %>%
  filter(!in_file) %>%
  distinct(Team, .keep_all = TRUE)

## URL updates for teams already in the file
url_updates <- matched %>%
  filter(in_file, URL != placeholder_url) %>%
  inner_join(select(teams247_old, Team, old_URL = URL), by = "Team") %>%
  filter(old_URL != URL) %>%
  distinct(Team, .keep_all = TRUE)

## Teams in the file that aren't on the current 247 page (left D1, dropped, etc.)
not_on_page <- teams247_old %>%
  filter(!Team %in% matched$Team)

## New teams whose key isn't a Team in team_database.rds (joins on Team will miss them)
not_in_db <- new_teams %>%
  filter(!Team %in% team_db$Team) %>%
  mutate(issue = if_else(Team %in% team_db$display_name[is.na(team_db$Team)],
                         "in team_database but Team is NA - fill in Team",
                         "not in team_database - add it"))

# Build updated table ----------------------------------------------------------
teams247_new <- teams247_old

if (update_urls && nrow(url_updates) > 0) {
  teams247_new <- teams247_new %>%
    rows_update(select(url_updates, Team, URL), by = "Team", unmatched = "ignore")
}

teams247_new <- teams247_new %>%
  bind_rows(select(new_teams, Team, URL)) %>%
  as.data.frame()

# Report -----------------------------------------------------------------------
cat("\n==== 247 team update ====\n")
cat(glue::glue("Existing teams: {nrow(teams247_old)} | Added: {nrow(new_teams)} | ",
               "URL updates: {if (update_urls) nrow(url_updates) else 0} | Final: {nrow(teams247_new)}"), "\n")

if (nrow(new_teams) > 0) {
  cat("\n-- Teams added --\n")
  new_teams %>%
    select(conference, team_247, Team, match_type, URL) %>%
    arrange(conference, Team) %>%
    print(n = Inf, width = Inf)
}

if (nrow(url_updates) > 0) {
  cat(glue::glue("\n-- URL changes for existing teams{if (!update_urls) ' (NOT applied, update_urls = FALSE)' else ''} --"), "\n")
  url_updates %>%
    select(Team, old_URL, new_URL = URL) %>%
    print(n = Inf, width = Inf)
}

if (nrow(not_in_db) > 0) {
  cat("\n-- Added teams not found as a Team in team_database.rds (check key / add to team_database) --\n")
  not_in_db %>%
    select(team_247, Team, match_type, issue) %>%
    print(n = Inf, width = Inf)
}

placeholder_teams <- teams247_new %>% filter(URL == placeholder_url)
if (nrow(placeholder_teams) > 0) {
  cat("\n-- Teams with no 247 team page (URL = https://247sports.com/, skip these in the recruiting scrape) --\n")
  cat(paste(" ", sort(placeholder_teams$Team)), sep = "\n")
}

if (nrow(not_on_page) > 0) {
  cat("\n-- In teams247.rds but not on the current 247 page (kept for historical joins) --\n")
  cat(paste(" ", sort(not_on_page$Team)), sep = "\n")
}

# Save -------------------------------------------------------------------------
if (save_changes) {
  saveRDS(teams247_new, teams247_path)
  message(glue::glue("\nSaved {nrow(teams247_new)} teams to {teams247_path}"))
} else {
  message("\nDry run - nothing saved (set save_changes <- TRUE to write the file)")
}
