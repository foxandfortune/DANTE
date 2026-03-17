library(tidyverse)
library(glmnet)

# Set working directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd('..')

# Set season/country/tier
season <- 2026

# Negate in formula
`%!in%` = Negate(`%in%`)

# Load teams -----
teams <- readRDS(glue::glue("_Helper Files/Team Data/team_database_wbb.rds"))

# Load summary data ----------
summary <- readRDS(glue::glue("BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Raw Data/poss_stats_with_types_wbb_{season}.rds")) %>% 
  # Filter out teams in team dataset
  filter(team_id %in% teams$team_id & opponent_team_id %in% teams$team_id) %>% 
  with_groups(game_id, mutate, tm_ct = n()) %>%
  # Only include games where both teams are in teams dataset
  filter(tm_ct == 2) %>% 
  select(-tm_ct) %>% 
  as.data.frame() %>% 
  rename(opp_id = opponent_team_id)

## Get games played/to be played for each team -----
games_played <- summary %>% 
  mutate(name = paste0("team_id_", team_id)) %>% 
  with_groups(.groups = name,
              summarise,
              games_played = n_distinct(game_id)) %>% 
  bind_rows(summary %>% 
              mutate(name = paste0("opp_id_", opp_id)) %>% 
              with_groups(.groups = name,
                          summarise,
                          games_played = n_distinct(game_id)))

### To be played -----------------------
schedule <- readRDS(glue::glue('_Helper Files/_WBB Fallback/updated_wbb_schedule_{season}.rds'))

schedule <- schedule %>% 
  select(game_date, game_id, team_id = home_id, opp_id = away_id) %>% 
  bind_rows(schedule %>% 
              select(game_date, game_id, team_id = away_id, opp_id = home_id))


games_to_play <- schedule %>% 
  mutate(name = paste0('team_id_', team_id)) %>% 
  with_groups(.groups = name,
              summarise,
              games_to_play = n_distinct(game_id)) %>% 
  bind_rows(schedule %>% 
              mutate(name = paste0('opp_id_', opp_id)) %>% 
              with_groups(.groups = name,
                          summarise,
                          games_to_play = n_distinct(game_id)))


# Load priors ---------------
priors.all <- readRDS(glue::glue('BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Priors/priors_wbb_{season}.rds'))

priors.pace <- priors.all$pace
priors.ast <- priors.all$ast
priors.oreb <- priors.all$oreb
priors.to <- priors.all$to
priors.rtg <- priors.all$rtg
priors.efg <- priors.all$efg
priors.rtg_raw <- priors.all$raw_rating

# Load create ratings function
create_ratings <- readRDS("_Helper Files/Other/create_ratings_stat.rds")


# Pace (Possessions per 40 minutes) -----------------------------------
stat_name <- "poss_per_40"

## Weights ----------
weights <- readRDS(glue::glue('BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights/single_season_wbb_{stat_name}_wgt.rds')) %>%
  rename(year = season) %>% 
  mutate(season_dist = abs(year - season)) %>% 
  filter(season_dist == min(season_dist))

weights_prior <- readRDS(glue::glue('VRGL/Stats/Team and Player Stats/Power Ratings/Weights/with_priors_{stat_name}_wgt.rds')) %>%
  rename(year = season) %>% 
  mutate(season_dist = abs(year - season)) %>% 
  filter(season_dist == min(season_dist))

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Weight data frame ----------------
weight.df <- data.frame(game_no = seq.int(from = 1,
                                          # Adjust max games as needed
                                          to = {weights_prior$max_games},
                                          by = 1)) %>%
  mutate(weight = {weights_prior$prior_wgt} ^ game_no,
         weight = weight / sum(weight),
         # Adjust by prior minimum as needed
         cum_weight = cumsum(weight) * (1 - weights_prior$prior_min)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)

## Load weights -----
wgt_var <- weights$wgt

print(wgt_var)

## Load lambda adj -----
lambda_adj <- weights$lambda_adj

print(lambda_adj)


## Create pace ratings ---------------------
raw.coeff.pace <- create_ratings(df = summary,
                            stat_name = {stat_name},
                            name_list = {list},
                            seed = 421,
                            wgt = {wgt_var},
                            lambda_adj = {lambda_adj})


# Add priors to create ratings ---------
ratings.pace <- priors.pace %>% 
  full_join(raw.coeff.pace,
            by = "name",
            suffix = c("_prior", "_curr")) %>% 
  left_join(games_played, by = "name") %>%
  mutate(games_played = case_when(
    is.na(games_played) ~ round(mean(games_played, na.rm = TRUE), 2),
    TRUE ~ games_played)) %>% 
  left_join(games_to_play, by = "name") %>% 
  left_join(weight.df, by = c("games_played" = "row")) %>% 
  mutate(cum_weight = replace_na(cum_weight,
                                 max(cum_weight, na.rm = TRUE))) %>% 
  mutate(value = case_when(
    is.na(value_curr) ~ value_prior,
    is.na(value_prior) ~ value_curr,
    str_detect(name, "team_id_") | str_detect(name, "opp_id_") ~ 
      cum_weight * value_curr + (1 - cum_weight) * value_prior,
    TRUE ~ (0.9 + 0.1 * games_played / weights_prior$max_games) * value_curr + 
      (0.1 - 0.1 * games_played / weights_prior$max_games) * value_prior)) %>%
  select(name, value)

head(ratings.pace)

## Clear stuff out  --------
rm(stat_name, list, wgt_var,
   priors.pace)

# Assist Rate -----------------------------------
stat_name <- "ast_rt"

## Weights ----------
weights <- readRDS(glue::glue('BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights/single_season_wbb_{stat_name}_wgt.rds')) %>%
  rename(year = season) %>% 
  mutate(season_dist = abs(year - season)) %>% 
  filter(season_dist == min(season_dist))

weights_prior <- readRDS(glue::glue('VRGL/Stats/Team and Player Stats/Power Ratings/Weights/with_priors_{stat_name}_wgt.rds')) %>%
  rename(year = season) %>% 
  mutate(season_dist = abs(year - season)) %>% 
  filter(season_dist == min(season_dist))

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Weight data frame ----------------
weight.df <- data.frame(game_no = seq.int(from = 1,
                                          # Adjust max games as needed
                                          to = {weights_prior$max_games},
                                          by = 1)) %>%
  mutate(weight = {weights_prior$prior_wgt} ^ game_no,
         weight = weight / sum(weight),
         # Adjust by prior minimum as needed
         cum_weight = cumsum(weight) * (1 - weights_prior$prior_min)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)

## Load weights -----
wgt_var <- weights$wgt

print(wgt_var)

## Load lambda adj -----
lambda_adj <- weights$lambda_adj

print(lambda_adj)

## Create assist rate ratings ---------------------
raw.coeff.ast <- create_ratings(df = summary,
                            stat_name = {stat_name},
                            name_list = {list},
                            seed = 421,
                            wgt = {wgt_var},
                            lambda_adj = {lambda_adj})


# Add priors to create ratings ---------
ratings.ast <- priors.ast %>% 
  full_join(raw.coeff.ast,
            by = "name",
            suffix = c("_prior", "_curr")) %>% 
  left_join(games_played, by = "name") %>%  
  mutate(games_played = case_when(
    is.na(games_played) ~ round(mean(games_played, na.rm = TRUE), 2),
    TRUE ~ games_played)) %>% 
  left_join(games_to_play, by = "name") %>% 
  left_join(weight.df, by = c("games_played" = "row")) %>% 
  mutate(cum_weight = replace_na(cum_weight,
                                 max(cum_weight, na.rm = TRUE))) %>% 
  mutate(value = case_when(
    is.na(value_curr) ~ value_prior,
    is.na(value_prior) ~ value_curr,
    str_detect(name, "team_id_") | str_detect(name, "opp_id_") ~ 
      cum_weight * value_curr + (1 - cum_weight) * value_prior,
    TRUE ~ (0.9 + 0.1 * games_played / weights_prior$max_games) * value_curr + 
      (0.1 - 0.1 * games_played / weights_prior$max_games) * value_prior)) %>% 
  select(name, value)

head(ratings.ast)

## Clear stuff out  --------
rm(stat_name, list, wgt_var,
   priors.ast)

# Offensive rebound rate -----------------------------------
stat_name <- "oreb_rt"

## Weights ----------
weights <- readRDS(glue::glue('BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights/single_season_wbb_{stat_name}_wgt.rds')) %>%
  rename(year = season) %>% 
  mutate(season_dist = abs(year - season)) %>% 
  filter(season_dist == min(season_dist))

weights_prior <- readRDS(glue::glue('VRGL/Stats/Team and Player Stats/Power Ratings/Weights/with_priors_{stat_name}_wgt.rds')) %>%
  rename(year = season) %>% 
  mutate(season_dist = abs(year - season)) %>% 
  filter(season_dist == min(season_dist))

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Weight data frame ----------------
weight.df <- data.frame(game_no = seq.int(from = 1,
                                          # Adjust max games as needed
                                          to = {weights_prior$max_games},
                                          by = 1)) %>%
  mutate(weight = {weights_prior$prior_wgt} ^ game_no,
         weight = weight / sum(weight),
         # Adjust by prior minimum as needed
         cum_weight = cumsum(weight) * (1 - weights_prior$prior_min)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)

## Load weights -----
wgt_var <- weights$wgt

print(wgt_var)

## Load lambda adj -----
lambda_adj <- weights$lambda_adj

print(lambda_adj)


## Create offensive rebround rate ratings ---------------------
raw.coeff.oreb <- create_ratings(df = summary,
                            stat_name = {stat_name},
                            name_list = {list},
                            seed = 421,
                            wgt = {wgt_var},
                            lambda_adj = {lambda_adj})


# Add priors to create ratings ---------
ratings.oreb <- priors.oreb %>% 
  full_join(raw.coeff.oreb,
            by = "name",
            suffix = c("_prior", "_curr")) %>% 
  left_join(games_played, by = "name") %>%  
  mutate(games_played = case_when(
    is.na(games_played) ~ round(mean(games_played, na.rm = TRUE), 2),
    TRUE ~ games_played)) %>% 
  left_join(games_to_play, by = "name") %>% 
  left_join(weight.df, by = c("games_played" = "row")) %>% 
  mutate(cum_weight = replace_na(cum_weight,
                                 max(cum_weight, na.rm = TRUE))) %>% 
  mutate(value = case_when(
    is.na(value_curr) ~ value_prior,
    is.na(value_prior) ~ value_curr,
    str_detect(name, "team_id_") | str_detect(name, "opp_id_") ~ 
      cum_weight * value_curr + (1 - cum_weight) * value_prior,
    TRUE ~ (0.9 + 0.1 * games_played / weights_prior$max_games) * value_curr + 
      (0.1 - 0.1 * games_played / weights_prior$max_games) * value_prior)) %>% 
  select(name, value)

head(ratings.oreb)

## Clear stuff out  --------
rm(stat_name, list, wgt_var,
   priors.oreb)

# Turnover Rate -----------------------------------
stat_name <- "to_rt"

## Weights ----------
weights <- readRDS(glue::glue('BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights/single_season_wbb_{stat_name}_wgt.rds')) %>%
  rename(year = season) %>% 
  mutate(season_dist = abs(year - season)) %>% 
  filter(season_dist == min(season_dist))

weights_prior <- readRDS(glue::glue('VRGL/Stats/Team and Player Stats/Power Ratings/Weights/with_priors_{stat_name}_wgt.rds')) %>%
  rename(year = season) %>% 
  mutate(season_dist = abs(year - season)) %>% 
  filter(season_dist == min(season_dist))

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Weight data frame ----------------
weight.df <- data.frame(game_no = seq.int(from = 1,
                                          # Adjust max games as needed
                                          to = {weights_prior$max_games},
                                          by = 1)) %>%
  mutate(weight = {weights_prior$prior_wgt} ^ game_no,
         weight = weight / sum(weight),
         # Adjust by prior minimum as needed
         cum_weight = cumsum(weight) * (1 - weights_prior$prior_min)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)

## Load weights -----
wgt_var <- weights$wgt

print(wgt_var)

## Load lambda adj -----
lambda_adj <- weights$lambda_adj

print(lambda_adj)

## Create turnover rate ratings ---------------------
raw.coeff.to <- create_ratings(df = summary,
                            stat_name = {stat_name},
                            name_list = {list},
                            seed = 421,
                            wgt = {wgt_var},
                            lambda_adj = {lambda_adj})

# Add priors to create ratings ---------
ratings.to <- priors.to %>% 
  full_join(raw.coeff.to,
            by = "name",
            suffix = c("_prior", "_curr")) %>% 
  left_join(games_played, by = "name") %>%  
  mutate(games_played = case_when(
    is.na(games_played) ~ round(mean(games_played, na.rm = TRUE), 2),
    TRUE ~ games_played)) %>% 
  left_join(games_to_play, by = "name") %>% 
  left_join(weight.df, by = c("games_played" = "row")) %>% 
  mutate(cum_weight = replace_na(cum_weight,
                                 max(cum_weight, na.rm = TRUE))) %>% 
  mutate(value = case_when(
    is.na(value_curr) ~ value_prior,
    is.na(value_prior) ~ value_curr,
    str_detect(name, "team_id_") | str_detect(name, "opp_id_") ~ 
      cum_weight * value_curr + (1 - cum_weight) * value_prior,
    TRUE ~ (0.9 + 0.1 * games_played / weights_prior$max_games) * value_curr + 
      (0.1 - 0.1 * games_played / weights_prior$max_games) * value_prior)) %>% 
  select(name, value)

head(ratings.to)

## Clear stuff out  --------
rm(stat_name, list, wgt_var,
   priors.to)

# Effective FG % -----------------------------------
stat_name <- "efg_adj"

## Weights ----------
weights <- readRDS(glue::glue('BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights/single_season_wbb_{stat_name}_wgt.rds')) %>%
  rename(year = season) %>% 
  mutate(season_dist = abs(year - season)) %>% 
  filter(season_dist == min(season_dist))

weights_prior <- readRDS(glue::glue('VRGL/Stats/Team and Player Stats/Power Ratings/Weights/with_priors_{stat_name}_wgt.rds')) %>%
  rename(year = season) %>% 
  mutate(season_dist = abs(year - season)) %>% 
  filter(season_dist == min(season_dist))

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Weight data frame ----------------
weight.df <- data.frame(game_no = seq.int(from = 1,
                                          # Adjust max games as needed
                                          to = {weights_prior$max_games},
                                          by = 1)) %>%
  mutate(weight = {weights_prior$prior_wgt} ^ game_no,
         weight = weight / sum(weight),
         # Adjust by prior minimum as needed
         cum_weight = cumsum(weight) * (1 - weights_prior$prior_min)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)

## Load weights -----
wgt_var <- weights$wgt

print(wgt_var)

## Load lambda adj -----
lambda_adj <- weights$lambda_adj

print(lambda_adj)

## Create EFG % ratings ---------------------
raw.coeff.efg <- create_ratings(df = summary,
                            stat_name = {stat_name},
                            name_list = {list},
                            seed = 421,
                            wgt = {wgt_var},
                            lambda_adj = {lambda_adj})

# Add priors to create ratings ---------
ratings.efg <- priors.efg %>% 
  full_join(raw.coeff.efg,
            by = "name",
            suffix = c("_prior", "_curr")) %>% 
  left_join(games_played, by = "name") %>%  
  mutate(games_played = case_when(
    is.na(games_played) ~ round(mean(games_played, na.rm = TRUE), 2),
    TRUE ~ games_played)) %>% 
  left_join(games_to_play, by = "name") %>% 
  left_join(weight.df, by = c("games_played" = "row")) %>% 
  mutate(cum_weight = replace_na(cum_weight,
                                 max(cum_weight, na.rm = TRUE))) %>% 
  mutate(value = case_when(
    is.na(value_curr) ~ value_prior,
    is.na(value_prior) ~ value_curr,
    str_detect(name, "team_id_") | str_detect(name, "opp_id_") ~ 
      cum_weight * value_curr + (1 - cum_weight) * value_prior,
    TRUE ~ (0.9 + 0.1 * games_played / weights_prior$max_games) * value_curr + 
      (0.1 - 0.1 * games_played / weights_prior$max_games) * value_prior)) %>% 
  select(name, value)


head(ratings.efg)

## Clear stuff out  --------
rm(stat_name, list, wgt_var,
   priors.efg)

# Team Ratings -----------------------------------------
stat_name <- "rtg"

## Weights ----------
weights <- readRDS(glue::glue('BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights/single_season_wbb_{stat_name}_wgt.rds')) %>%
  rename(year = season) %>% 
  mutate(season_dist = abs(year - season)) %>% 
  filter(season_dist == min(season_dist))

weights_prior <- readRDS(glue::glue('VRGL/Stats/Team and Player Stats/Power Ratings/Weights/with_priors_{stat_name}_wgt.rds')) %>%
  rename(year = season) %>% 
  mutate(season_dist = abs(year - season)) %>% 
  filter(season_dist == min(season_dist))

## Features ----------
list <- c('game_date', 'team_id', 'opp_id',
          'poss_per_40', 'ast_rt', 'oreb_rt', 'to_rt',
          'is_home', 'neutral_site', 'days_rest', 'travel')


## Weight data frame ----------------
weight.df <- data.frame(game_no = seq.int(from = 1,
                                          # Adjust max games as needed
                                          to = {weights_prior$max_games},
                                          by = 1)) %>%
  mutate(weight = {weights_prior$prior_wgt} ^ game_no,
         weight = weight / sum(weight),
         # Adjust by prior minimum as needed
         cum_weight = cumsum(weight) * (1 - weights_prior$prior_min)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)

## Load weights -----
wgt_var <- weights$wgt

print(wgt_var)

## Load lambda adj -----
lambda_adj <- weights$lambda_adj

print(lambda_adj)

# Create Rtg ratings ---------------------
raw.coeff.rtg <- create_ratings(df = summary,
                            stat_name = {stat_name},
                            name_list = {list},
                            seed = 421,
                            wgt = {wgt_var},
                            lambda_adj = {lambda_adj})


# Add priors to create ratings ---------
ratings.rtg <- priors.rtg %>% 
  full_join(raw.coeff.rtg,
            by = "name",
            suffix = c("_prior", "_curr")) %>% 
  left_join(games_played, by = "name") %>%  
  mutate(games_played = case_when(
    is.na(games_played) ~ round(mean(games_played, na.rm = TRUE), 2),
    TRUE ~ games_played)) %>% 
  left_join(games_to_play, by = "name") %>% 
  left_join(weight.df, by = c("games_played" = "row")) %>% 
  mutate(cum_weight = replace_na(cum_weight,
                                 max(cum_weight, na.rm = TRUE))) %>% 
  mutate(value = case_when(
    is.na(value_curr) ~ value_prior,
    is.na(value_prior) ~ value_curr,
    str_detect(name, "team_id_") | str_detect(name, "opp_id_") ~ 
      cum_weight * value_curr + (1 - cum_weight) * value_prior,
    TRUE ~ (0.9 + 0.1 * games_played / weights_prior$max_games) * value_curr + 
      (0.1 - 0.1 * games_played / weights_prior$max_games) * value_prior)) %>% 
  select(name, value)

## Look to see who's in the top 68 -------------
ratings.rtg %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(ratings.rtg %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  left_join(teams %>% 
              select(team_id, team), by = "team_id") %>% 
  select(team, OffRtg, DefRtg) %>% 
  mutate(Total = OffRtg - DefRtg) %>% 
  arrange(desc(Total)) %>% 
  head(68)

## Compare to priors 
priors.rtg %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(priors.rtg %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  left_join(teams %>% 
              select(team_id, team), by = "team_id") %>% 
  select(team, OffRtg, DefRtg) %>% 
  mutate(Total = OffRtg - DefRtg) %>% 
  arrange(desc(Total)) %>% 
  head(68)

## Compare to raw this season only 
raw.coeff.rtg %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(raw.coeff.rtg %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  left_join(teams %>% 
              select(team_id, team), by = "team_id") %>% 
  select(team, OffRtg, DefRtg) %>% 
  mutate(Total = OffRtg - DefRtg) %>% 
  arrange(desc(Total)) %>% 
  head(68)

# Clear stuff out
rm(stat_name, list, wgt_var,
   priors.rtg)


# Raw Team Ratings (DO NOT INCLUDE OTHER STATS) -----------------------------------------
stat_name <- "rtg"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id',
          'poss_per_40',
          'is_home', 'neutral_site', 'days_rest', 'travel')

## Load weights -----
wgt_var <- weights$wgt

print(wgt_var)

## Load lambda adj -----
lambda_adj <- weights$lambda_adj

print(lambda_adj)


# Create Rtg ratings ---------------------
raw.coeff.rtg.raw <- create_ratings(df = summary,
                                    stat_name = {stat_name},
                                    name_list = {list},
                                    seed = 421,
                                    wgt = {wgt_var},
                                    lambda_adj = {lambda_adj})

# Add priors to create ratings ---------
ratings.rtg_raw <- priors.rtg_raw %>% 
  full_join(raw.coeff.rtg.raw,
            by = "name",
            suffix = c("_prior", "_curr")) %>% 
  left_join(games_played, by = "name") %>%  
  mutate(games_played = case_when(
    is.na(games_played) ~ round(mean(games_played, na.rm = TRUE), 2),
    TRUE ~ games_played)) %>% 
  left_join(games_to_play, by = "name") %>% 
  left_join(weight.df, by = c("games_played" = "row")) %>% 
  mutate(cum_weight = replace_na(cum_weight,
                                 max(cum_weight, na.rm = TRUE))) %>% 
  mutate(value = case_when(
    is.na(value_curr) ~ value_prior,
    is.na(value_prior) ~ value_curr,
    str_detect(name, "team_id_") | str_detect(name, "opp_id_") ~ 
      cum_weight * value_curr + (1 - cum_weight) * value_prior,
    TRUE ~ (0.9 + 0.1 * games_played / weights_prior$max_games) * value_curr + 
      (0.1 - 0.1 * games_played / weights_prior$max_games) * value_prior)) %>% 
  select(name, value)

## Look to see who's in the top 68 -------------
ratings.rtg_raw %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(ratings.rtg_raw %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  left_join(teams %>% 
              select(team_id, team), by = "team_id") %>% 
  select(team, OffRtg, DefRtg) %>% 
  mutate(Total = OffRtg - DefRtg) %>% 
  arrange(desc(Total)) %>% 
  head(68)

## Compare to priors 
priors.rtg_raw %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(priors.rtg_raw %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  left_join(teams %>% 
              select(team_id, team), by = "team_id") %>% 
  select(team, OffRtg, DefRtg) %>% 
  mutate(Total = OffRtg - DefRtg) %>% 
  arrange(desc(Total)) %>% 
  head(68)

## Compare to raw this season only 
raw.coeff.rtg.raw %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(raw.coeff.rtg.raw %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  left_join(teams %>% 
              select(team_id, team), by = "team_id") %>% 
  select(team, OffRtg, DefRtg) %>% 
  mutate(Total = OffRtg - DefRtg) %>% 
  arrange(desc(Total)) %>% 
  head(68)

# Combine all ratings in to single list --------------------
rtgs.all <- list(pace = {ratings.pace},
                 ast = {ratings.ast},
                 oreb = {ratings.oreb},
                 to = {ratings.to},
                 rtg = {ratings.rtg},
                 #Additional for site
                 efg = {ratings.efg},
                 raw_rating = {ratings.rtg_raw})

# Ratings with no priors ------------------------
rtgs.no_prior <- list(pace = {raw.coeff.pace},
                      ast = {raw.coeff.ast},
                      oreb = {raw.coeff.oreb},
                      to = {raw.coeff.to},
                      rtg = {raw.coeff.rtg},
                      #Additional for site
                      efg = {raw.coeff.efg},
                      raw_rating = {raw.coeff.rtg.raw})

## Save ratings --------
saveRDS(rtgs.all,
        glue::glue("BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_wbb_{season}.rds"))
Sys.setFileTime(glue::glue("BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_wbb_{season}.rds"),
                Sys.time()) 


saveRDS(rtgs.no_prior,
        glue::glue("BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_no_prior_wbb_{season}.rds"))
Sys.setFileTime(glue::glue("BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_no_prior_wbb_{season}.rds"),
                Sys.time()) 
