# Data loading -----------------------------------------------------------------

# Team-game rows exactly as the production in-season script prepares them
load_summary <- function(league, season) {
  teams <- readRDS(bt_path(league, "teams"))
  readRDS(bt_path(league, "poss", season = season)) %>%
    filter(team_id %in% teams$team_id & opponent_team_id %in% teams$team_id) %>%
    group_by(game_id) %>% mutate(tm_ct = n()) %>% ungroup() %>%
    filter(tm_ct == 2) %>%
    select(-tm_ct) %>%
    as.data.frame() %>%
    rename(opp_id = opponent_team_id) %>%
    mutate(game_date = as.Date(game_date),
           is_home = as.logical(is_home),
           neutral_site = as.logical(neutral_site))
}

# One row per game (home perspective; for neutral games the lower team_id is
# "home" unless the data says otherwise) with outcomes and pre-game context
games_from_summary <- function(summary) {
  s <- summary %>%
    select(game_id, game_date, team_id, opp_id, team_score, is_home,
           neutral_site, days_rest, travel, poss_per_40, rtg)
  s %>%
    inner_join(s %>% select(game_id, opp_id = team_id, opp_score = team_score,
                            opp_rest = days_rest, opp_travel = travel,
                            opp_is_home = is_home, opp_rtg = rtg),
               by = c("game_id", "opp_id")) %>%
    # choose the "home" row: is_home TRUE, else (neutral) lower team_id
    mutate(pick = case_when(is_home ~ TRUE,
                            opp_is_home ~ FALSE,
                            TRUE ~ team_id < opp_id)) %>%
    filter(pick) %>%
    transmute(game_id, game_date,
              home_id = team_id, away_id = opp_id,
              neutral = neutral_site,
              home_rest = days_rest, away_rest = opp_rest,
              home_dist = travel, away_dist = opp_travel,
              home_score = team_score, away_score = opp_score,
              margin = team_score - opp_score,
              total = team_score + opp_score,
              pace_act = poss_per_40,
              home_rtg_act = rtg, away_rtg_act = opp_rtg) %>%
    distinct(game_id, .keep_all = TRUE)
}

# BT_PRIORS_DIR: read priors from another folder (same file names), e.g. priors
# rebuilt by priors/_Build Priors.R
load_priors <- function(league, season) {
  f <- bt_path(league, "priors", season = season)
  pd <- Sys.getenv("BT_PRIORS_DIR", "")
  if (nzchar(pd)) f <- file.path(pd, basename(f))
  if (!file.exists(f)) return(NULL)
  readRDS(f)
}

load_weights <- function(league, stat, season) {
  pick <- function(f) {
    readRDS(f) %>% rename(year = season) %>%
      mutate(season_dist = abs(year - !!season)) %>%
      filter(season_dist == min(season_dist)) %>% slice(1)
  }
  list(single = pick(bt_path(league, "w_single", stat = stat)),
       prior  = pick(bt_path(league, "w_prior", stat = stat)))
}
