library(tidyverse)
library(glmnet)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

# Set season/country/tier
season <- 2025

# Negate in formula
`%!in%` = Negate(`%in%`)

# Load teams -----
teams <- readRDS(glue::glue("Teams/team_database.rds"))

# Load summary data ----------
summary <- readRDS(glue::glue("Power Ratings/Raw Data/poss_stats_with_types_{season}.rds")) %>% 
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

## To be played
schedule <- hoopR::load_mbb_schedule(seasons = {season})

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


# Create weights for priors
max_games <- games_to_play %>% 
  filter(!str_detect(name, '-1'), !str_detect(name, '-2')) %>% 
  filter(games_to_play == max(games_to_play)) %>% 
  select(-name) %>% 
  distinct() %>% 
  pull(games_to_play)

print(max_games)

# Load priors ---------------
priors.all <- readRDS(glue::glue('Power Ratings/Team Ratings/Priors/priors_{season}.rds'))

priors.pace <- priors.all$pace
priors.ast <- priors.all$ast
priors.oreb <- priors.all$oreb
priors.to <- priors.all$to
priors.rtg <- priors.all$rtg
priors.efg <- priors.all$efg
priors.rtg_raw <- priors.all$raw_rating

# Load create ratings function
create_ratings <- readRDS("Tools/create_ratings_stat.rds")

summary %>% 
  mutate(team_name = paste0('team_id_', team_id),
         opp_name = paste0('opp_id_', opp_id)) %>% 
  select(game_id, game_date,
         team_id, team_name, opp_id, opp_name,
         days_rest, is_home, neutral_site, travel,
         poss_per_40, ast_rt, oreb_rt, to_rt, rtg, efg_adj) %>%
  # Add pace priors
  left_join(priors.pace, by = c("team_name" = "name")) %>% 
  rename(team_pace = value) %>% 
  left_join(priors.pace, by = c("opp_name" = "name")) %>% 
  rename(opp_pace = value) %>%
  # Add assist priors
  left_join(priors.ast, by = c("team_name" = "name")) %>% 
  rename(team_ast = value) %>% 
  left_join(priors.ast, by = c("opp_name" = "name")) %>% 
  rename(opp_ast = value) %>%
  # Add oreb priors
  left_join(priors.oreb, by = c("team_name" = "name")) %>% 
  rename(team_oreb = value) %>% 
  left_join(priors.oreb, by = c("opp_name" = "name")) %>% 
  rename(opp_oreb = value) %>%
  # Add turnover priors
  left_join(priors.to, by = c("team_name" = "name")) %>% 
  rename(team_to = value) %>% 
  left_join(priors.to, by = c("opp_name" = "name")) %>% 
  rename(opp_to = value) %>%
  # Add rating priors
  left_join(priors.rtg, by = c("team_name" = "name")) %>% 
  rename(team_rtg = value) %>% 
  left_join(priors.rtg, by = c("opp_name" = "name")) %>% 
  rename(opp_rtg = value) %>%
  # Add efg priors
  left_join(priors.efg, by = c("team_name" = "name")) %>% 
  rename(team_efg = value) %>% 
  left_join(priors.efg, by = c("opp_name" = "name")) %>% 
  rename(opp_efg = value)
  











# Pace (Possessions per 40 minutes) -----------------------------------
stat_name <- "poss_per_40"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Weight data frame ----------------
weight.df <- data.frame(game_no = seq.int(from = 1,
                                          # Adjust max games as needed
                                          to = max_games - 0,
                                          by = 1)) %>%
  mutate(weight = 0.7 ^ game_no,
         weight = weight / sum(weight),
         # Adjust by prior minimum as needed
         cum_weight = cumsum(weight) * (1 - 0.10)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)

## Load weights -----
wgt_var <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  rename(year = season) %>%
  filter(year <= season) %>% 
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(wgt = round(weighted.mean(x = wgt, w = weight), 3)) %>% 
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  rename(year = season) %>%
  filter(year <= season) %>% 
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(lambda_adj = weighted.mean(x = lambda_adj, w = weight)) %>% 
  select(lambda_adj) %>% 
  as.double()

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
    TRUE ~ (0.9 + 0.1 * games_played / max_games) * value_curr + 
      (0.1 - 0.1 * games_played / max_games) * value_prior)) %>%
  select(name, value)

head(ratings.pace)

## Clear stuff out  --------
rm(stat_name, list, wgt_var,
   priors.pace)

# Assist Rate -----------------------------------
stat_name <- "ast_rt"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Weight data frame ----------------
weight.df <- data.frame(game_no = seq.int(from = 1,
                                          # Adjust max games as needed
                                          to = max_games - 0,
                                          by = 1)) %>%
  mutate(weight = 0.9 ^ game_no,
         weight = weight / sum(weight),
         # Adjust by prior minimum as needed
         cum_weight = cumsum(weight) * (1 - 0.10)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)

## Load weights -----
wgt_var <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  mutate(wgt = as.double(wgt), lambda_adj = as.double(lambda_adj)) %>% 
  rename(year = season) %>%
  filter(year <= season) %>% 
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(wgt = round(weighted.mean(x = wgt, w = weight), 3)) %>% 
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  mutate(wgt = as.double(wgt), lambda_adj = as.double(lambda_adj)) %>% 
  rename(year = season) %>%
  filter(year <= season) %>% 
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(lambda_adj = weighted.mean(x = lambda_adj, w = weight)) %>% 
  select(lambda_adj) %>% 
  as.double()

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
    TRUE ~ (0.9 + 0.1 * games_played / max_games) * value_curr + 
      (0.1 - 0.1 * games_played / max_games) * value_prior)) %>% 
  select(name, value)

head(ratings.ast)

## Clear stuff out  --------
rm(stat_name, list, wgt_var,
   priors.ast)

# Offensive rebound rate -----------------------------------
stat_name <- "oreb_rt"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Weight data frame ----------------
weight.df <- data.frame(game_no = seq.int(from = 1,
                                          # Adjust max games as needed
                                          to = max_games - 2,
                                          by = 1)) %>%
  mutate(weight = 0.9 ^ game_no,
         weight = weight / sum(weight),
         # Adjust by prior minimum as needed
         cum_weight = cumsum(weight) * (1 - 0.15)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)

## Load weights -----
wgt_var <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  mutate(wgt = as.double(wgt), lambda_adj = as.double(lambda_adj)) %>% 
  rename(year = season) %>%
  filter(year <= season) %>% 
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(wgt = round(weighted.mean(x = wgt, w = weight), 3)) %>% 
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  mutate(wgt = as.double(wgt), lambda_adj = as.double(lambda_adj)) %>% 
  rename(year = season) %>%
  filter(year <= season) %>% 
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(lambda_adj = weighted.mean(x = lambda_adj, w = weight)) %>% 
  select(lambda_adj) %>% 
  as.double()

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
    TRUE ~ (0.9 + 0.1 * games_played / max_games) * value_curr + 
      (0.1 - 0.1 * games_played / max_games) * value_prior)) %>% 
  select(name, value)

head(ratings.oreb)

## Clear stuff out  --------
rm(stat_name, list, wgt_var,
   priors.oreb)

# Turnover Rate -----------------------------------
stat_name <- "to_rt"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Weight data frame ----------------
weight.df <- data.frame(game_no = seq.int(from = 1,
                                          # Adjust max games as needed
                                          to = max_games - 0,
                                          by = 1)) %>%
  mutate(weight = 0.83 ^ game_no,
         weight = weight / sum(weight),
         # Adjust by prior minimum as needed
         cum_weight = cumsum(weight) * (1 - 0.10)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)

## Load weights -----
wgt_var <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  mutate(wgt = as.double(wgt), lambda_adj = as.double(lambda_adj)) %>% 
  rename(year = season) %>%
  filter(year <= season) %>% 
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(wgt = round(weighted.mean(x = wgt, w = weight), 3)) %>% 
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  mutate(wgt = as.double(wgt), lambda_adj = as.double(lambda_adj)) %>% 
  rename(year = season) %>%
  filter(year <= season) %>% 
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(lambda_adj = weighted.mean(x = lambda_adj, w = weight)) %>% 
  select(lambda_adj) %>% 
  as.double()

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
    TRUE ~ (0.9 + 0.1 * games_played / max_games) * value_curr + 
      (0.1 - 0.1 * games_played / max_games) * value_prior)) %>% 
  select(name, value)

head(ratings.to)

## Clear stuff out  --------
rm(stat_name, list, wgt_var,
   priors.to)

# Effective FG % -----------------------------------
stat_name <- "efg_adj"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Weight data frame ----------------
weight.df <- data.frame(game_no = seq.int(from = 1,
                                          # Adjust max games as needed
                                          to = max_games - 0,
                                          by = 1)) %>%
  mutate(weight = 0.7 ^ game_no,
         weight = weight / sum(weight),
         # Adjust by prior minimum as needed
         cum_weight = cumsum(weight) * (1 - 0.10)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)

## Load weights -----
wgt_var <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  mutate(wgt = as.double(wgt), lambda_adj = as.double(lambda_adj)) %>% 
  rename(year = season) %>%
  filter(year <= season) %>% 
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(wgt = round(weighted.mean(x = wgt, w = weight), 3)) %>% 
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  mutate(wgt = as.double(wgt), lambda_adj = as.double(lambda_adj)) %>% 
  rename(year = season) %>%
  filter(year <= season) %>% 
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(lambda_adj = weighted.mean(x = lambda_adj, w = weight)) %>% 
  select(lambda_adj) %>% 
  as.double()

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
    TRUE ~ (0.9 + 0.1 * games_played / max_games) * value_curr + 
      (0.1 - 0.1 * games_played / max_games) * value_prior)) %>% 
  select(name, value)


head(ratings.efg)

## Clear stuff out  --------
rm(stat_name, list, wgt_var,
   priors.efg)

# Team Ratings -----------------------------------------
stat_name <- "rtg"

## Weight data frame ----------------
weight.df <- data.frame(game_no = seq.int(from = 1,
                                          # Adjust max games as needed
                                          to = max_games - 3,
                                          by = 1)) %>%
  mutate(weight = 0.95 ^ game_no,
         weight = weight / sum(weight),
         # Adjust by prior minimum as needed
         cum_weight = cumsum(weight) * (1 - 0.15)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)


## Features ----------
list <- c('game_date', 'team_id', 'opp_id',
          'poss_per_40', 'ast_rt', 'oreb_rt', 'to_rt',
          'is_home', 'neutral_site', 'days_rest', 'travel')

# Load weights -----
wgt_var <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  mutate(wgt = as.double(wgt)) %>% 
  rename(year = season) %>%
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(year <= season) %>% 
  filter(rank == 1, year != 2021) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(wgt = weighted.mean(x = wgt, w = weight)) %>% 
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- 0.75 #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  #mutate(wgt = as.double(wgt), lambda_adj = as.double(lambda)) %>% 
  #rename(year = season) %>%
  #filter(year <= season) %>% 
  #with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  #filter(rank == 1) %>%
  #mutate(weight = .97^(season - as.numeric(year))) %>% 
  #summarise(lambda_adj = weighted.mean(x = lambda_adj, w = weight)) %>% 
  #select(lambda_adj) %>% 
  #as.double()

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
    TRUE ~ (0.9 + 0.1 * games_played / max_games) * value_curr + 
      (0.1 - 0.1 * games_played / max_games) * value_prior)) %>% 
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

# Load weights -----
wgt_var <- 0.97 #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_raw_weights.rds')) %>%
  #mutate(wgt = as.double(wgt)) %>% 
  #rename(year = season) %>%
  #with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  #filter(year == season) %>% 
  #filter(rank == 1) %>%
  #mutate(weight = .97^(season - as.numeric(year))) %>% 
  #summarise(wgt = weighted.mean(x = wgt, w = weight)) %>% 
  #select(wgt) %>% 
  #as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- 0.75 #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  #mutate(wgt = as.double(wgt), lambda_adj = as.double(lambda)) %>% 
  #rename(year = season) %>%
  #filter(year <= season) %>% 
  #with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  #filter(rank == 1) %>%
  #mutate(weight = .97^(season - as.numeric(year))) %>% 
  #summarise(lambda_adj = weighted.mean(x = lambda_adj, w = weight)) %>% 
  #select(lambda_adj) %>% 
  #as.double()

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
    TRUE ~ (0.9 + 0.1 * games_played / max_games) * value_curr + 
      (0.1 - 0.1 * games_played / max_games) * value_prior)) %>% 
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
        glue::glue("Power Ratings/Team Ratings/Inseason/inseason_ratings_all_{season}.rds"))

saveRDS(rtgs.no_prior,
        glue::glue("Power Ratings/Team Ratings/Inseason/inseason_ratings_all_no_prior_{season}.rds"))
