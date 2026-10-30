library(tidyverse)
library(rvest)

# Scrapes each conference's ESPN standings page and saves one row per team:
# team_id, season, conf_id. Writes mbb_conf_{season}.rds / wbb_conf_{season}.rds.
#
# The script is safe to re-run from the top: every result is built fresh inside
# functions (nothing is appended to objects left over from an earlier run), and
# the files are only written after the duplicate and coverage checks pass.

# Set season --------
# Find the DANTE folder: from this script's location when it is open in RStudio,
# else from the working directory (Rscript, GitHub Actions). The DANTE project
# doesn't need to be open.
dante_root <- local({
  starts <- c(tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) ""), getwd())
  for (s in starts[nzchar(starts)]) {
    r <- tryCatch(rprojroot::find_root(rprojroot::has_file("DANTE.Rproj"), path = s), error = function(e) NULL)
    if (!is.null(r)) return(r)
  }
  stop("Can't find the DANTE folder (DANTE.Rproj): open the DANTE project or setwd() into it")
})
dante_path <- function(...) file.path(dante_root, ...)
source(dante_path("_Helper Files", "season.R"))
season <- dante_season

conf_dir <- dante_path('_Helper Files/Team Data/Conferences')

# Load conference IDs ----
# Every ID is scraped every season. A conference that doesn't exist in a given
# season/league (Independent, Pac-12 in 2025-26, Summit 47 for men / 49 for
# women) just returns an empty page, so no season filters are needed. The old
# filter dropped the Pac-12 for every season after 2024, which loses its 9
# teams from 2027 on.
conf_ids <- read.csv(file.path(conf_dir, 'cbb_conference_ids.csv'))

# Helpers ------
read_espn <- function(url, tries = 3) {
  for (k in seq_len(tries)) {
    page <- tryCatch(read_html(url), error = function(e) e)
    if (!inherits(page, 'error')) return(page)
    message('  attempt ', k, ' failed: ', conditionMessage(page))
    Sys.sleep(10 * k)
  }
  stop('Could not read ', url)
}

team_ids_on_page <- function(page) {
  # Only links inside the standings table(s). ESPN sometimes serves a sidebar
  # with links to ~16 top teams on every page, which put those teams into
  # almost every conference (and into empty ones like Independent).
  tibble(href = page %>% html_elements('.standings__table a') %>% html_attr('href')) %>%
    filter(str_detect(href, 'team/_/id/')) %>%
    transmute(team_id = str_extract(href, '(?<=/id/)\\d+')) %>%
    filter(!is.na(team_id)) %>%
    # distinct on the ID, not the link: one team can have several hrefs
    distinct(team_id)
}

standings_url <- function(league, season, conf_id = NULL) {
  base <- glue::glue('https://www.espn.com/{league}-college-basketball/standings/_/season/{season}/')
  if (is.null(conf_id)) base else glue::glue('{base}group/{conf_id}')
}

scrape_league <- function(league, season, conf_ids, pause = 3) {
  map(conf_ids$conf_id, \(id) {
    out <- read_espn(standings_url(league, season, id)) %>%
      team_ids_on_page() %>%
      mutate(season = season, conf_id = id)
    Sys.sleep(pause)
    out
  }, .progress = league) %>%
    list_rbind()
}

stop_if_layout_changed <- function(conf, league) {
  # If ESPN renames the standings table class, the selector finds nothing
  if (n_distinct(conf$team_id) < 300) {
    stop(league, ': only ', n_distinct(conf$team_id), ' teams found. ',
         'Check that .standings__table still exists on the ESPN pages.')
  }
}

check_league <- function(conf, league, season, conf_ids) {
  # 1. No team may sit in more than one conference
  dups <- conf %>%
    add_count(team_id) %>%
    filter(n > 1) %>%
    left_join(conf_ids, by = 'conf_id') %>%
    arrange(team_id)
  if (nrow(dups) > 0) {
    print(dups, n = Inf)
    stop(league, ': ', n_distinct(dups$team_id), ' team(s) listed in more than one conference')
  }
  
  # 2. Every team on the all-conference standings page should be assigned
  all_ids <- read_espn(standings_url(league, season)) %>% team_ids_on_page()
  missing <- anti_join(all_ids, conf, by = 'team_id')
  if (nrow(missing) > 0) {
    warning(league, ': ', nrow(missing), ' team(s) on the full standings page have no conference: ',
            paste(missing$team_id, collapse = ', '))
  }
  
  # 3. Team counts by conference, for eyeballing
  conf %>%
    count(conf_id) %>%
    left_join(conf_ids, by = 'conf_id') %>%
    arrange(conference_short_name) %>%
    print(n = Inf)
  
  message(league, ' ', season, ': ', nrow(conf), ' teams in ', n_distinct(conf$conf_id), ' conferences')
  invisible(conf)
}

# Scrape ------
mbb_conf <- scrape_league('mens', season, conf_ids)
wbb_conf <- scrape_league('womens', season, conf_ids)

# 2020-21 only: ESPN has no WBB standings for CUSA, MEAC, Patriot or Sun Belt,
# and the Ivy League didn't play. The saved 2021 files already include these
# patches, so this only matters if 2021 is re-scraped.
if (season == 2021) {
  ivy_2023 <- readRDS(file.path(conf_dir, 'wbb_conf_2023.rds')) %>%
    filter(conf_id == 12) %>%
    mutate(season = 2021)
  
  # Assigns the unplaced teams by their order on the full standings page
  # (14 CUSA, 8 MEAC, 10 Patriot, 12 Sun Belt). Check this by eye before saving.
  missing_wbb <- read_espn(standings_url('womens', season)) %>%
    team_ids_on_page() %>%
    anti_join(bind_rows(wbb_conf, ivy_2023), by = 'team_id')
  stopifnot(nrow(missing_wbb) == 44)
  missing_wbb <- missing_wbb %>%
    mutate(season = 2021,
           conf_id = c(rep(11, 14), rep(16, 8), rep(22, 10), rep(27, 12)))
  
  wbb_conf <- bind_rows(wbb_conf, ivy_2023, missing_wbb)
}

# Check ------
stop_if_layout_changed(mbb_conf, 'mens')
stop_if_layout_changed(wbb_conf, 'womens')
check_league(mbb_conf, 'mens', season, conf_ids)
check_league(wbb_conf, 'womens', season, conf_ids)

# Save (only reached if both checks pass) ------
saveRDS(as.data.frame(mbb_conf), file.path(conf_dir, glue::glue('mbb_conf_{season}.rds')))
saveRDS(as.data.frame(wbb_conf), file.path(conf_dir, glue::glue('wbb_conf_{season}.rds')))