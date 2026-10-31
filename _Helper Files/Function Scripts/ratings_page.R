# ──────────────────────────────────────────────────────────────────────────────
# ratings_page.R
#
# Shared pieces for the ratings pages in Pages/ (vrgl_ratings*.qmd, btrc_ratings*.qmd):
#
#   season_links(league, page_season)   the "Other Seasons" line, built from the pages
#                                       that exist, so a new season shows up by itself
#   season_honors(scoreboard, conf)     seed, conference regular-season leader/champion,
#                                       conference tournament champion and national
#                                       champion for every team, from the hoopR/wehoop
#                                       schedule; updates as the season goes on
#   season_records(scoreboard, standings)  W-L for every team
#   season_finished(scoreboard)         TRUE once the national championship is played
#   season_status(scoreboard, season, preseason)  "Preseason ratings ..." / "... through <date>" /
#                                       "Final ratings ..."
#   ratings_stat_table(ratings_all)     one row per team: pace, rebound/turnover/assist
#                                       rates, eFG, ORtg/DRtg/NetRtg from a ratings_all file
#   pick_ratings_file(league, season, finished)  which ratings file a page shows
#   ratings_are_priors(league, season, ratings_all)  TRUE while the ratings are still the priors
#   dante_try(expr)                     run expr, return NULL (with a message) if it errors
#
# Everything here is written to keep the site building when ESPN or the sportsdataverse
# data is missing or late: no schedule means no honors and a "Preseason" status,
# not an error.
# ──────────────────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({ library(dplyr); library(stringr) })

if (!exists("dante_season")) source(here::here("_Helper Files", "season.R"))

dante_try <- function(expr) {
  tryCatch(expr, error = function(e) {
    message("ratings_page: ", conditionMessage(e))
    NULL
  })
}

# "Other Seasons" line ---------------------------------------------------------
# league: "vrgl" or "btrc". page_season: the season of the page it goes on.
# Archive pages link only if their .qmd exists in Pages/; the current season
# (dante_season) links to vrgl_ratings.html / btrc_ratings.html.
season_links <- function(league, page_season = dante_season, first_season = 2021) {
  lab <- c(vrgl = "MBB", btrc = "WBB")[[league]]
  parts <- vapply(first_season:dante_season, function(y) {
    txt <- sprintf("%d-%02d", y - 1, y %% 100)
    if (y == page_season) return(txt)
    if (y == dante_season) {
      page <- sprintf("%s_ratings.html", league)
    } else {
      if (!file.exists(here::here("Pages", sprintf("%s_ratings_%d.qmd", league, y)))) return(NA_character_)
      page <- sprintf("%s_ratings_%d.html", league, y)
    }
    sprintf('[%s](https://foxandfortune.github.io/DANTE/Pages/%s "%d %s Ratings")', txt, page, y, lab)
  }, character(1))
  paste("[***Other Seasons:***]{.underline}", paste(parts[!is.na(parts)], collapse = " **/** "))
}

# Schedule helpers -------------------------------------------------------------
.empty_sched <- function(scoreboard) is.null(scoreboard) || nrow(scoreboard) == 0

.completed <- function(scoreboard) {
  scoreboard %>%
    filter(status_type_completed %in% TRUE, !is.na(home_score), !is.na(away_score)) %>%
    mutate(home_id = as.character(home_id), away_id = as.character(away_id),
           win_id = if_else(home_score > away_score, home_id, away_id))
}

season_finished <- function(scoreboard) {
  if (.empty_sched(scoreboard)) return(FALSE)
  nc <- .completed(scoreboard) %>%
    filter(tournament_id %in% 22, str_detect(notes_headline, "National Championship"))
  nrow(nc) > 0
}

# preseason = TRUE when the ratings shown are still the priors (see ratings_are_priors),
# e.g. games have started but the daily run hasn't updated the ratings yet.
season_status <- function(scoreboard, season, preseason = FALSE) {
  yrs <- sprintf("%d-%02d", season - 1, season %% 100)
  if (season_finished(scoreboard)) return(glue::glue("Final ratings for the {yrs} season."))
  done <- if (.empty_sched(scoreboard)) NULL else .completed(scoreboard)
  if (preseason || is.null(done) || nrow(done) == 0) return(glue::glue("Preseason ratings for the {yrs} season."))
  last <- max(as.Date(substr(as.character(done$game_date), 1, 10)), na.rm = TRUE)
  glue::glue("Ratings for the {yrs} season, through games of {format(last, '%B')} {as.integer(format(last, '%d'))}.")
}

# W-L records ------------------------------------------------------------------
# Uses ESPN standings (espn_mbb_standings / espn_wbb_standings, column total) when
# they are there; otherwise counts wins and losses in the schedule. Teams with no
# games yet get 0-0.
season_records <- function(scoreboard, standings = NULL, team_ids) {
  rec <- NULL
  if (!is.null(standings) && nrow(standings) > 0 && all(c("team_id", "total") %in% names(standings))) {
    rec <- standings %>% transmute(team_id = as.character(team_id), total = as.character(total)) %>%
      distinct(team_id, .keep_all = TRUE)
  } else if (!.empty_sched(scoreboard)) {
    g <- .completed(scoreboard)
    rec <- bind_rows(g %>% transmute(team_id = home_id, w = home_score > away_score),
                     g %>% transmute(team_id = away_id, w = away_score > home_score)) %>%
      group_by(team_id) %>%
      summarise(total = paste0(sum(w), "-", sum(!w)), .groups = "drop")
  }
  tibble(team_id = as.character(team_ids)) %>%
    left_join(if (is.null(rec)) tibble(team_id = character(), total = character()) else rec, by = "team_id") %>%
    mutate(total = coalesce(total, "0-0"))
}

# Honors -----------------------------------------------------------------------
# conf: data frame with team_id and conference_short_name for the season (from the
# DANTE conference file). Returns one row per team in conf with:
#   seed        "(n)" once the NCAA bracket is out, else ""
#   reg_season  "star" = conference regular-season leader (champion once conference
#               play is over), "star-half" = tied for it, else ""
#   conf_tourn  "trophy" = won the conference tournament final, else ""
#   title       "trophy" = national champion, else ""
season_honors <- function(scoreboard, conf) {
  conf <- conf %>% transmute(team_id = as.character(team_id), conf = conference_short_name) %>%
    filter(!is.na(conf)) %>% distinct(team_id, .keep_all = TRUE)
  out <- conf %>% transmute(team_id, seed = "", reg_season = "", conf_tourn = "", title = "")
  if (.empty_sched(scoreboard)) return(out)
  done <- .completed(scoreboard)

  ## Conference regular season: conference games outside any tournament, both teams
  ## in the same DANTE conference. Leaders = best conference win %, then most
  ## conference wins (ties share it). Once every team has played out the conference
  ## schedule this is just the regular-season champion(s).
  reg <- done %>%
    filter(conference_competition %in% TRUE, is.na(tournament_id)) %>%
    inner_join(conf %>% rename(home_id = team_id, home_conf = conf), by = "home_id") %>%
    inner_join(conf %>% rename(away_id = team_id, away_conf = conf), by = "away_id") %>%
    filter(home_conf == away_conf)
  if (nrow(reg) == 0) {
    leaders <- tibble(team_id = character(), reg_season_new = character())
  } else leaders <- bind_rows(reg %>% transmute(team_id = home_id, conf = home_conf, w = home_score > away_score),
                       reg %>% transmute(team_id = away_id, conf = away_conf, w = away_score > home_score)) %>%
    group_by(conf, team_id) %>%
    summarise(wins = sum(w), games = n(), .groups = "drop") %>%
    mutate(pct = wins / games) %>%
    group_by(conf) %>%
    filter(abs(pct - max(pct)) < 1e-9) %>%
    filter(wins == max(wins)) %>%   # early on, 3-0 leads 2-0
    mutate(n_lead = n()) %>%
    ungroup() %>%
    transmute(team_id, reg_season_new = if_else(n_lead == 1, "star", "star-half"))

  ## Conference tournament finals ("... - Final"), completed
  ct <- done %>%
    filter(conference_competition %in% TRUE, !is.na(tournament_id),
           str_detect(coalesce(notes_headline, ""), "- Final$")) %>%
    distinct(win_id) %>% transmute(team_id = win_id, conf_tourn_new = "trophy")

  ## NCAA tournament seeds (tournament_id 22) and national champion
  ncaa <- scoreboard %>% filter(tournament_id %in% 22)
  seeds <- bind_rows(ncaa %>% transmute(team_id = as.character(home_id), seed = home_current_rank),
                     ncaa %>% transmute(team_id = as.character(away_id), seed = away_current_rank)) %>%
    filter(!is.na(seed), seed >= 1, seed <= 16) %>%
    distinct(team_id, .keep_all = TRUE) %>%
    transmute(team_id, seed_new = paste0("(", seed, ")"))
  champ <- done %>%
    filter(tournament_id %in% 22, str_detect(notes_headline, "National Championship")) %>%
    transmute(team_id = win_id, title_new = "trophy")

  out %>%
    left_join(leaders, by = "team_id") %>%
    left_join(ct, by = "team_id") %>%
    left_join(seeds, by = "team_id") %>%
    left_join(champ, by = "team_id") %>%
    transmute(team_id,
              seed       = coalesce(seed_new, ""),
              reg_season = coalesce(reg_season_new, ""),
              conf_tourn = coalesce(conf_tourn_new, ""),
              title      = coalesce(title_new, ""))
}

# Which ratings file a page shows ------------------------------------------------
# Finished season: the full-season fit. Otherwise the in-season file, which is the
# preseason priors until the daily run replaces it. NULL if neither exists yet
# (e.g. after the season rolls over but before step 6 of the offseason update).
pick_ratings_file <- function(league, season, finished) {
  tag <- if (league == "btrc") "wbb_" else ""
  base <- if (league == "btrc") "BTRC/Stats/Team and Player Stats - WBB" else "VRGL/Stats/Team and Player Stats"
  full <- here::here(base, "Power Ratings", "Team Ratings", "Full Season", sprintf("ratings_all_%s%d.rds", tag, season))
  ins  <- here::here(base, "Power Ratings", "Team Ratings", "Inseason", sprintf("inseason_ratings_all_%s%d.rds", tag, season))
  if (finished && file.exists(full)) return(full)
  if (file.exists(ins)) return(ins)
  if (file.exists(full)) return(full)
  NULL
}

# Are these ratings still the preseason priors? -------------------------------------
# The in-season starter file is a copy of the priors until the daily run refits it.
ratings_are_priors <- function(league, season, ratings_all) {
  tag <- if (league == "btrc") "wbb_" else ""
  base <- if (league == "btrc") "BTRC/Stats/Team and Player Stats - WBB" else "VRGL/Stats/Team and Player Stats"
  pf <- here::here(base, "Power Ratings", "Team Ratings", "Priors", sprintf("priors_%s%d.rds", tag, season))
  if (!file.exists(pf)) return(FALSE)
  pri <- readRDS(pf)$raw_rating
  m <- merge(ratings_all$raw_rating, pri, by = "name")
  nrow(m) == nrow(ratings_all$raw_rating) && isTRUE(all.equal(m$value.x, m$value.y))
}

# Conference file for a season (falls back to the latest earlier one) ---------------
season_conferences <- function(league, season) {
  lg <- if (league == "btrc") "wbb" else "mbb"
  dir <- here::here("_Helper Files", "Team Data", "Conferences")
  yrs <- as.integer(str_match(list.files(dir, sprintf("^%s_conf_\\d{4}\\.rds$", lg)), "(\\d{4})")[, 2])
  yr <- max(yrs[yrs <= season])
  readRDS(file.path(dir, sprintf("%s_conf_%d.rds", lg, yr))) %>%
    left_join(read.csv(file.path(dir, "cbb_conference_ids.csv")) %>% select(conf_id, conference_short_name), by = "conf_id") %>%
    transmute(team_id = as.character(team_id), conference_short_name) %>%
    as.data.frame()
}

# Team stat table from a ratings_all file ------------------------------------------
# Same math the pages have always used: offense/defense = intercept + team/opponent
# coefficient; pace = mean of the two; ORtg/DRtg adjusted for pace.
ratings_stat_table <- function(ratings_all) {
  side <- function(r, prefix) {
    r %>% filter(str_detect(name, paste0("^", prefix))) %>%
      transmute(team_id = str_replace_all(str_remove(name, prefix), "_", " "), value)
  }
  both <- function(r) {
    base <- r$value[r$name == "(Intercept)"]
    side(r, "team_id_") %>% rename(off = value) %>%
      left_join(side(r, "opp_id_") %>% rename(def = value), by = "team_id") %>%
      mutate(off = off + base, def = def + base)
  }
  pace_adj <- ratings_all$raw_rating$value[ratings_all$raw_rating$name == "poss_per_40"]
  both(ratings_all$pace) %>% transmute(team_id, pace = (off + def) / 2) %>%
    left_join(both(ratings_all$ast)  %>% transmute(team_id, ast_rt = off), by = "team_id") %>%
    left_join(both(ratings_all$efg)  %>% transmute(team_id, tm_efg = off, opp_efg = def), by = "team_id") %>%
    left_join(both(ratings_all$oreb) %>% transmute(team_id, oreb_rt = off, dreb_rt = def), by = "team_id") %>%
    left_join(both(ratings_all$to)   %>% transmute(team_id, to_rt = off, to_opp_rt = def), by = "team_id") %>%
    left_join(both(ratings_all$raw_rating) %>% transmute(team_id, ortg = off, drtg = def), by = "team_id") %>%
    mutate(ortg = ortg + pace * pace_adj,
           drtg = drtg + pace * pace_adj,
           netrtg = ortg - drtg)
}

# Legend for the honors columns (only when something is showing) -------------------
honors_legend <- function(honors) {
  if (is.null(honors) || !any(c(honors$seed, honors$reg_season, honors$conf_tourn, honors$title) != "")) return("")
  paste("<span style='font-size: 0.85em;'>Star: conference regular-season champion",
        "(leader while conference play is going; half star = tied).",
        "Gray trophy: conference tournament champion. Gold trophy: national champion.",
        "(n): NCAA tournament seed.</span>")
}
