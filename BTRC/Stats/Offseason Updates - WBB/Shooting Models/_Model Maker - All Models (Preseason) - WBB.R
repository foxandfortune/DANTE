# packages
library(fastDummies)
library(splitTools)
library(dials)
library(xgboost)
library(ggplot2)
library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Add NOT IN function:
`%!in%` = Negate(`%in%`)

# this should be the default u y do this R
options(scipen = 999999)

# Set reference year
cur_yr <- 2025

# Load data
all_shots <- readRDS(glue::glue("Shot Files/pbp_raw_current.rds"))

unique(all_shots$season)

## Filter for prior four seasons
all_shots <- all_shots %>% 
  filter(season >= {cur_yr - 4})

# Move working directory up ----
setwd('..')

######################## xgboost for location ########################
set.seed(421)

# Load xgboost best model
best_model <- readRDS(glue::glue("Models/xfg_xgboost.rds"))

# Set parameters with results from best model
# Include how man rounds to train for
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = best_model$eta,
    gamma = best_model$gamma,
    subsample = best_model$subsample,
    colsample_bytree = best_model$colsample_bytree,
    max_depth = best_model$max_depth, # Manually adjusting because I think this might help
    min_child_weight = best_model$min_child_weight,
    monotone_constraints = best_model$monotone_constraints
  )

nrounds <- best_model$iter

# Select features and labels
shots_model <- all_shots %>% 
  filter(shooting_play == TRUE, type_id != "540", !is.na(shot_dist)) %>% 
  mutate(pos = case_when(
    pos != "NA" & !is.na(pos) ~ pos,
    is.na(pos) ~ "ATH",
    pos == "NA" ~ "ATH"
  )) %>% 
  mutate(shot_diff_time = if_else(secs_remaining == 0, shot_diff/.1, shot_diff / secs_remaining),
         year_index = as.numeric(season) - cur_yr,
         pos = as.factor(pos),
         shot_made_numeric = as.numeric(scoring_play),
         season_type = as.factor(season_type)) %>% 
  select(
    # Labels and ids
    label = shot_made_numeric, season,
    game_id, game_play_number,
    period_display_value, period_number,
    team_id, poss_tm, poss_id, poss_row, poss_start,
    player_id, other_player_id, player, number, athlete_headshot_href,
    away_team_id, home_team_id,
    point_value, shot_value,
    
    # Features
    shot_dist, shot_angle,
    x, y, pos, is_atrim, is_putback, 
    is_home, secs_remaining,
    shot_diff, shot_diff_time,
    shot_clock, is_fastbreak, year_index,
    season_type) %>% 
  # Create dummy variables for position and shot type
  # because xgboost doesn't like these for some reason
  fastDummies::dummy_cols(select_columns = c("pos", "season_type"), remove_first_dummy = TRUE) %>% 
  # Drop extra columns from dummy variables
  select(-c(pos, season_type))

# Get train data
train_data <- shots_model %>%
  dplyr::filter(season < cur_yr)

# Get labels
train_labels <- train_data %>%
  dplyr::select(label, game_id, game_play_number,
                period_display_value, period_number,
                team_id, poss_tm, poss_id, poss_row, poss_start,
                player_id, other_player_id, player, number, athlete_headshot_href,
                away_team_id, home_team_id,
                point_value, shot_value)

# get rid of extra columns
train_data <- train_data %>%
  dplyr::select(-c(label, game_id, game_play_number,
                   period_display_value, period_number,
                   team_id, poss_tm, poss_id, poss_row, poss_start,
                   player_id, other_player_id, player, number, athlete_headshot_href,
                   away_team_id, home_team_id,
                   point_value, shot_value))

# Train xgboost model using best parameters from above
xfg_model <- xgboost::xgboost(
  params = params,
  data = as.matrix(train_data),
  label = train_labels$label,
  nrounds = nrounds,
  verbose = 2
)

# Save model for each year
xgb.save(xfg_model, glue::glue("Models/xfg {cur_yr}.model"))

rm(best_model, params, 
   shots_model, train_data, train_labels,
   xfg_model, nrounds)

######################## xgboost for non-location ########################
set.seed(421)

# Load xgboost best model
best_model <- readRDS(glue::glue("Models/xfg_xgboost_no_loc.rds"))

# Set paramangers with results from best model
# Include how man rounds to train for
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = best_model$eta,
    gamma = best_model$gamma,
    subsample = best_model$subsample,
    colsample_bytree = best_model$colsample_bytree,
    max_depth = best_model$max_depth, # Manually adjusting because I think this might help
    min_child_weight = best_model$min_child_weight,
    monotone_constraints = best_model$monotone_constraints
  )

nrounds <- best_model$iter

# Select features and labels
shots_model <- all_shots %>% 
  filter(shooting_play == TRUE, type_id != "540", !is.na(pos), is.na(shot_dist), pos != "NA") %>% 
  mutate(shot_diff_time = if_else(secs_remaining == 0, shot_diff/.1, shot_diff / secs_remaining),
         year_index = as.numeric(season) - cur_yr,
         pos = as.factor(pos),
         shot_made_numeric = as.numeric(scoring_play),
         season_type = as.factor(season_type),
         is_three = case_when(
           type_id %in% c("30558", "558") ~ TRUE,
           str_detect(text, "Three Point") ~ TRUE,
           TRUE ~ FALSE)) %>% 
  select(
    # Labels and ids
    label = shot_made_numeric, season,
    game_id, game_play_number,
    period_display_value, period_number,
    team_id, poss_tm, poss_id, poss_row, poss_start,
    player_id, other_player_id, player, number, athlete_headshot_href,
    away_team_id, home_team_id,
    point_value, shot_value,
    
    # Features
    pos, is_atrim, is_putback, is_three, 
    is_home, secs_remaining,
    shot_diff, shot_diff_time,
    shot_clock, is_fastbreak, year_index,
    season_type) %>% 
  # Create dummy variables for position and shot type
  # because xgboost doesn't like these for some reason
  fastDummies::dummy_cols(select_columns = c("pos", "season_type"), remove_first_dummy = TRUE) %>% 
  # Drop extra columns from dummy variables
  select(-c(pos, season_type))

# Get train data
train_data <- shots_model %>%
  dplyr::filter(season < cur_yr)

# Get labels
train_labels <- train_data %>%
  dplyr::select(label, game_id, game_play_number,
                period_display_value, period_number,
                team_id, poss_tm, poss_id, poss_row, poss_start,
                player_id, other_player_id, player, number, athlete_headshot_href,
                away_team_id, home_team_id,
                point_value, shot_value)

# get rid of extra columns
train_data <- train_data %>%
  dplyr::select(-c(label, game_id, game_play_number,
                   period_display_value, period_number,
                   team_id, poss_tm, poss_id, poss_row, poss_start,
                   player_id, other_player_id, player, number, athlete_headshot_href,
                   away_team_id, home_team_id,
                   point_value, shot_value))

# Train xgboost model using best parameters from above
xfg_model <- xgboost::xgboost(
  params = params,
  data = as.matrix(train_data),
  label = train_labels$label,
  nrounds = nrounds,
  verbose = 2
)

# Save model for each year
xgb.save(xfg_model, glue::glue("Models/xfg noloc {cur_yr}.model"))

rm(best_model, params, 
   shots_model, train_data, train_labels,
   xfg_model, nrounds)

# Filter out only free throws
fts <- all_shots %>% 
  filter(shooting_play == TRUE, type_id == "540",
         season < cur_yr) %>% 
  mutate(pos = case_when(
    pos != "NA" ~ pos,
    is.na(pos) ~ "ATH",
    pos == "NA" ~ "ATH"
  ))

# Group free throws by position
ft_model <- fts %>% 
  mutate(pos = case_when(
    pos == "F-C" ~ "F",
    pos == "G-F" ~ "G",
    TRUE ~ pos
  )) %>% 
  with_groups(.groups = pos, summarise, fts = sum(shot_value),
              xFG = sum(point_value)/sum(shot_value))

ft_model

# Save
saveRDS(ft_model, glue::glue("Models/ft {cur_yr}.rds"))
