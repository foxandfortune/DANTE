library(tidyverse)
library(gt)
library(gtExtras)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Add NOT IN function:
`%!in%` = Negate(`%in%`)

# Set season
season <- 2025

# Dante header function
gt_dante_title <- readRDS('Minos/Simulation Backup/Functions/gt_dante_title.rds')

# Check to see if season is finished -------------
season_comp_status <- hoopR::load_mbb_schedule(2025) %>% 
  filter(tournament_id == 22,
         str_detect(notes_headline, "National Championship")) %>% 
  pull(status_type_completed)

# Load ratings 
if(season_comp_status == TRUE){
  ratings_all <- readRDS(glue::glue('Stats/Power Ratings/Team Ratings/Full Season/ratings_all_{season}.rds'))
} else {
  ratings_all <- readRDS(glue::glue('Stats/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_{season}.rds'))
}


ratings.pace <- ratings_all$pace
ratings.ast <- ratings_all$ast
ratings.to <- ratings_all$to
ratings.reb <- ratings_all$oreb
ratings.efg <- ratings_all$efg
ratings.rtg <- ratings_all$raw_rating

# Load Teams
teams <- readRDS('Stats/Teams/team_database.rds')
conf <- cbbreadr::load_teams(2025) %>%
  left_join(cbbreadr::load_conferences() %>% 
              select(conference_id = id,
                     short_name), by = 'conference_id') %>% 
  transmute(team_id = as.numeric(source_id),
            conference_short_name = short_name) %>% 
  as.data.frame()

team_logos <- hoopR::espn_mbb_teams(year = 2025) %>% 
  select(team_id, logo) %>% 
  left_join(conf, by = 'team_id')

teams <- teams %>% 
  left_join(team_logos %>% 
              mutate(team_id = as.character(team_id)),
            by = 'team_id') %>% 
  filter(team_id != '107554') %>% 
  mutate(logo = case_when(
    is.na(logo) ~ paste0('https://a.espncdn.com/i/teamlogos/ncaa/500/',
                         team_id,
                         '.png'),
    TRUE ~ logo),
    conference_short_name = case_when(
      is.na(conference_short_name) & team == 'Queens University' ~ 'ASUN',
      is.na(conference_short_name) & team %in% c('Lindenwood',
                                                 'Southern Indiana') ~ 'OVC',
      TRUE ~ conference_short_name)) %>% 
  filter(!is.na(conference_short_name))


# Get base ratings for all stats + adjustment for pace -----------
base.pace <- ratings.pace$value[ratings.pace$name == "(Intercept)"]
pace_adj <- ratings.rtg$value[ratings.rtg$name == 'poss_per_40']
base.ast <- ratings.ast$value[ratings.ast$name == "(Intercept)"]
base.to <- ratings.to$value[ratings.to$name == "(Intercept)"]
base.reb <- ratings.reb$value[ratings.reb$name == "(Intercept)"]
base.efg <- ratings.efg$value[ratings.efg$name == "(Intercept)"]
base.rtg <- ratings.rtg$value[ratings.rtg$name == "(Intercept)"]


# Make team ratings tables for each stat ----
## Pace -------------
tm.pace <- ratings.pace %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(ratings.pace %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  select(team_id, OffRtg, DefRtg) %>% 
  mutate(OffRtg = OffRtg + base.pace,
         DefRtg = DefRtg + base.pace) %>% 
  mutate(pace = (OffRtg + DefRtg) / 2) %>% 
  select(-c(OffRtg, DefRtg))

## Assist rate -------------
tm.ast <- ratings.ast %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  select(team_id, OffRtg) %>% 
  mutate(OffRtg = OffRtg + base.ast)

## Turnover rate -------------
tm.to <- ratings.to %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(ratings.to %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  select(team_id, OffRtg, DefRtg) %>% 
  mutate(OffRtg = OffRtg + base.to,
         DefRtg = DefRtg + base.to) 

## Rebound rate ---------------
tm.reb <- ratings.reb %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(ratings.reb %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  select(team_id, OffRtg, DefRtg) %>% 
  mutate(OffRtg = OffRtg + base.reb,
         DefRtg = DefRtg + base.reb)

## Effective FG % / True Shooting ----------
tm.efg <- ratings.efg %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(ratings.efg %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  select(team_id, OffRtg, DefRtg) %>% 
  mutate(OffRtg = OffRtg + base.efg,
         DefRtg = DefRtg + base.efg)

## Rating -------------
tm.rtg <- ratings.rtg %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(ratings.rtg %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  select(team_id, OffRtg, DefRtg) %>% 
  mutate(OffRtg = OffRtg + base.rtg,
         DefRtg = DefRtg + base.rtg) 

# Get results from season --------------------------------
results <- hoopR::espn_mbb_standings(2025)

## Team W-L records --------------
records <- results %>% 
  transmute(team_id = as.character(team_id),
            total)

## Regular season champions / leaders (10 + Conf games played) -----------
reg_season_champ <- results %>% 
  mutate(team_id = as.character(team_id)) %>%  
  filter(vsconf_gamesbehind == 0,
         vsconf_wins + vsconf_losses >= 10) %>% 
  select(team_id, team, total, vsconf) %>% 
  left_join(teams %>% 
              select(team_id, conf = conference_short_name),
            by = 'team_id') %>% 
  arrange(conf) %>% 
  with_groups(.groups = conf,
              mutate,
              champs = n()) %>% 
  transmute(team_id = as.character(team_id),
            champs)

## Load scoreboard for Conference Tourney / National Champions -----------
scoreboard <- hoopR::load_mbb_schedule(2025)

## Conference tournament ------------
conf_tourn_champ <- scoreboard %>% 
  filter(conference_competition == TRUE,
         str_detect(notes_headline, 'Final')) %>% 
  transmute(tournament_id,
            conf_id = home_conference_id,
            win_id = case_when(
              away_score > home_score ~ as.character(away_id),
              home_score > away_score ~ as.character(home_id)
            )) %>% 
  select(win_id, conf_id)

## Tournament Seeds -------------
seeds <- scoreboard %>% 
  filter(tournament_id == 22) %>% 
  select(team_id = home_id, seed = home_current_rank) %>% 
  bind_rows(scoreboard %>% 
              filter(tournament_id == 22) %>% 
              select(team_id = away_id, seed = away_current_rank)) %>% 
  distinct() %>% 
  mutate(team_id = as.character(team_id))

## National Champion ------------
national_champ <- scoreboard %>% 
  filter(tournament_id == 22,
         str_detect(notes_headline, 'National Championship')) %>% 
  transmute(win_id = case_when(
    home_score > away_score ~ as.character(home_id),
    away_score > home_score ~ as.character(away_id)
  ))

# Create full ratings table ---------------------------------------------- 
ratings_tbl <- tm.pace %>% 
  left_join(tm.ast, by = 'team_id') %>%
  rename(ast_rt = OffRtg) %>% 
  left_join(tm.efg, by = 'team_id') %>% 
  rename(tm_efg = OffRtg,
         opp_efg = DefRtg) %>% 
  left_join(tm.reb, by = 'team_id') %>% 
  rename(oreb_rt = OffRtg,
         dreb_rt = DefRtg) %>% 
  left_join(tm.to, by = 'team_id') %>% 
  rename(to_rt = OffRtg,
         to_opp_rt = DefRtg) %>% 
  left_join(tm.rtg, by = 'team_id') %>% 
  rename(ortg = OffRtg,
         drtg = DefRtg) %>% 
  mutate(ortg = ortg + pace * pace_adj,
         drtg = drtg + pace * pace_adj,
         netrtg = ortg - drtg) %>% 
  left_join(teams %>% 
              select(team_id, team,
                     logo, conference_short_name), by = "team_id") %>% 
  arrange(desc(netrtg)) %>% 
  left_join(seeds, by = 'team_id') %>% 
  left_join(records, by = 'team_id') %>% 
  left_join(reg_season_champ, by = 'team_id') %>% 
  left_join(conf_tourn_champ, by = c('team_id' = 'win_id')) %>% 
  mutate(title = case_when(
    team_id %in% national_champ$win_id ~ 'trophy',
    TRUE ~ ''),
    seed = case_when(
      is.na(seed) ~ "",
      !is.na(seed) ~ paste0("(",seed,")")),
    reg_season = case_when(
      champs == 1 ~ 'star',
      champs > 1 ~ 'star-half',
      TRUE ~ ''),
    conf_tourn = case_when(
      !is.na(conf_id) ~ 'trophy',
      TRUE ~ ''
    )) %>%
  filter(!is.na(conference_short_name)) %>% 
  mutate(rank = dense_rank(desc(netrtg))) %>% 
  select(rank, logo, team, seed,
         reg_season, conf_tourn, title,
         conference_short_name, total,
         pace, oreb_rt, dreb_rt,
         to_rt, to_opp_rt, ast_rt,
         tm_efg, opp_efg,
         ortg, drtg, netrtg)
  

# Make table header ------------------
gt_title <- gt_dante_title(title = glue::glue("{season} VRGL Ratings for Men's Basketball"),
                           subtitle = "Defense-Adjusted Net Team Efficiency (D.A.N.T.E.)",
                           filepath = '',
                           type = "mbb",
                           logo_height = 75)


# Create table -------------------------
create_ratings_table(ratings_tbl)

