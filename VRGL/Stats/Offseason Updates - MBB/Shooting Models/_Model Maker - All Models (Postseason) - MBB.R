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

# Load data
all_shots <- readRDS(glue::glue("Shot Files/pbp_raw_current.rds"))

unique(all_shots$season)

# Set reference year
cur_yr <- max(all_shots$season)

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

test_data <- shots_model %>%
  dplyr::filter(season == cur_yr)

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

# Predict on holdout set
preds <- stats::predict(
  xfg_model,
  # get rid of the things not needed for prediction here
  as.matrix(test_data %>% select(-c(label, game_id, game_play_number,
                                    period_display_value, period_number,
                                    team_id, poss_tm, poss_id, poss_row, poss_start,
                                    player_id, other_player_id, player, number, athlete_headshot_href,
                                    away_team_id, home_team_id,
                                    point_value, shot_value)
  ))
) %>%
  tibble::as_tibble() %>%
  dplyr::rename(xfg = value) %>%
  dplyr::bind_cols(test_data)

preds

# Model evaluation
MLmetrics::LogLoss(preds$xfg, preds$label)

# Calibration plot
plot <- preds %>%
  # Create BINS for xfg:
  dplyr::mutate(bin_pred_prob = round(xfg / 0.05) * .05) %>%
  dplyr::group_by(bin_pred_prob) %>%
  # Calculate the calibration results:
  dplyr::summarize(
    n_plays = n(),
    n_wins = length(which(label == 1)),
    bin_actual_prob = n_wins / n_plays
  ) %>%
  dplyr::ungroup()

ann_text <- data.frame(
  x = c(.25, 0.75), y = c(0.75, 0.25),
  lab = c("Made more times\nthan expected", "Made fewer times\nthan expected")
)

plot %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    size = "Number of shots",
    x = "Estimated FG percentage",
    y = "Observed FG percentage",
    title = "xFG calibration plot - With Location"
  ) +
  geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 2) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )

# Save model for each year
xgb.save(xfg_model, glue::glue("Models/xfg {cur_yr}.model"))

rm(ann_text, best_model, params, plot, preds,
   shots_model, test_data, train_data, train_labels,
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

test_data <- shots_model %>%
  dplyr::filter(season == cur_yr)

length(train_data$label) + length(test_data$label) == length(shots_model$label)

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

# Predict on holdout set
preds <- stats::predict(
  xfg_model,
  # get rid of the things not needed for prediction here
  as.matrix(test_data %>% select(-c(label, game_id, game_play_number,
                                    period_display_value, period_number,
                                    team_id, poss_tm, poss_id, poss_row, poss_start,
                                    player_id, other_player_id, player, number, athlete_headshot_href,
                                    away_team_id, home_team_id,
                                    point_value, shot_value)
  ))
) %>%
  tibble::as_tibble() %>%
  dplyr::rename(xfg = value) %>%
  dplyr::bind_cols(test_data)

preds

# Model evaluation
MLmetrics::LogLoss(preds$xfg, preds$label)

# Calibration plot
plot <- preds %>%
  # Create BINS for xfg:
  dplyr::mutate(bin_pred_prob = round(xfg / 0.05) * .05) %>%
  dplyr::group_by(bin_pred_prob) %>%
  # Calculate the calibration results:
  dplyr::summarize(
    n_plays = n(),
    n_wins = length(which(label == 1)),
    bin_actual_prob = n_wins / n_plays
  ) %>%
  dplyr::ungroup()

ann_text <- data.frame(
  x = c(.25, 0.75), y = c(0.75, 0.25),
  lab = c("Made more times\nthan expected", "Made fewer times\nthan expected")
)

plot %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    size = "Number of shots",
    x = "Estimated FG percentage",
    y = "Observed FG percentage",
    title = "xFG calibration plot - No Location"
  ) +
  geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 2) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )


# Save model for each year
xgb.save(xfg_model, glue::glue("Models/xfg noloc {cur_yr}.model"))

rm(ann_text, best_model, params, plot, preds,
   shots_model, test_data, train_data, train_labels,
   xfg_model, nrounds)

##################### FREE THROW MODEL ##################################
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
