# packages
library(fastDummies)
library(splitTools)
library(dials)
library(xgboost)
library(ggplot2)
library(tidyverse)

set.seed(421)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Add NOT IN function:
`%!in%` = Negate(`%in%`)

# this should be the default u y do this R
options(scipen = 999999)

# size of hyperparameter grid to search over
# if you don't have a potato computer, can set this to a bigger number
grid_size <- 50

# Set reference year
cur_yr <- 2022

# Load data
all_shots <- readRDS(glue::glue("PBP and Shot Data/all_shots_2018_{cur_yr}.rds")) %>% 
  filter(season >= (cur_yr - 4))

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

# Get test and train data
test_data <- shots_model %>%
  dplyr::filter(season == cur_yr)

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


grid <- dials::grid_latin_hypercube(
  # this finalize thing is because mtry depends on # of columns in data
  dials::finalize(dials::mtry(), train_data),
  dials::min_n(),
  dials::tree_depth(),
  # to force learn_rate to not be crazy small like dials defaults to
  # because my computer is slow
  # if you're trying this for a different problem, expand the range here
  # by using more negative values
  dials::learn_rate(range = c(-1.45, -0.75), trans = scales::log10_trans()),
  dials::loss_reduction(),
  sample_size = dials::sample_prop(),
  size = grid_size
) %>%
  dplyr::mutate(
    # has to be between 0 and 1 for xgb
    # for some reason mtry gives the number of columns rather than proportion
    mtry = mtry / length(train_data),
    monotone_constraints = "(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0)"
  ) %>%
  # make these the right names for xgb
  dplyr::rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )

grid

# function to perform xgb.cv for a given row in a hyperparameter grid
get_row <- function(row) {
  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = row$eta,
      gamma = row$gamma,
      subsample = row$subsample,
      colsample_bytree = row$colsample_bytree,
      max_depth = row$max_depth,
      min_child_weight = row$min_child_weight,
      monotone_constraints = row$monotone_constraints
    )
  
  # do the cross validation
  xfg_cv_model <- xgboost::xgb.cv(
    data = as.matrix(train_data),
    label = train_labels$label,
    params = params,
    # this doesn't matter with early stopping in xgb.cv, just set a big number
    # the actual optimal rounds will be found in this tuning process
    nrounds = 15000,
    # created above
    nfold = 5,
    metrics = list("logloss"),
    early_stopping_rounds = 50,
    print_every_n = 50
  )
  
  # bundle up the results together for returning
  output <- params
  output$iter <- xfg_cv_model$best_iteration
  output$logloss <- xfg_cv_model$evaluation_log[output$iter]$test_logloss_mean
  
  row_result <- bind_rows(output)
  
  return(row_result)
}

# get results
results <- purrr::map_df(1:nrow(grid), function(x) {
  get_row(grid %>% dplyr::slice(x))
})

saveRDS(results, "results.rds")

results = readRDS("results_v2.rds")

results %>%
  dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  tidyr::pivot_longer(
    eta:min_child_weight,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, logloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "logloss") +
  theme_minimal()

# Retune based on outputs
grid <- dials::grid_latin_hypercube(
  # colsample_bytree
  dials::mtry(range = c(5, length(train_data))), # First number based on ratio an num of variables
  dials::min_n(),
  # force tree depth to be between 3 and 5
  dials::tree_depth(range = c(4L, 9L)), # max_depth
  # to force learn_rate to not be crazy small like dials defaults to
  dials::learn_rate(range = c(-1.5, -.8), trans = scales::log10_trans()), # eta (10 ^ p)
  dials::loss_reduction(),
  sample_size = dials::sample_prop(),
  size = grid_size
) %>%
  dplyr::mutate(
    # has to be between 0 and 1 for xgb
    # for some reason mtry gives the number of columns rather than proportion
    mtry = mtry / length(train_data),
    monotone_constraints = "(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0)"
  ) %>%
  # make these the right names for xgb
  dplyr::rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )

grid

# get results with new grid
results <- purrr::map_df(1:nrow(grid), function(x) {
  get_row(grid %>% dplyr::slice(x))
})

saveRDS(results, "results v2.rds")

# and plot
results %>%
  dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  tidyr::pivot_longer(eta:min_child_weight,
                      values_to = "value",
                      names_to = "parameter"
  ) %>%
  ggplot(aes(value, logloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "logloss") +
  theme_minimal()

saveRDS(results, "results_v3.1.rds")

# Test new monotone constraints
#grid2 <- grid %>%
#  dplyr::mutate(
#    # old
#    # monotone_constraints = "(0, 0, 0, 0, 0, 1, 1, -1, -1, -1, 1, -1)"
#    
#    # new
#    monotone_constraints = "(0, 1, 0, 0, 0, 1, 1, -1, -1, -1, 1, -1)"
#  )

#results2 <- purrr::map_df(1:nrow(grid2), function(x) {
#  get_row(grid2 %>% dplyr::slice(x))
#})

#glue::glue(
#  "--BEST LOGLOSS--
#
#No monotone constraint on spread_time:
#{round(results %>% arrange(logloss) %>% dplyr::slice(1) %>% pull(logloss), 5)}
#
#Monotone constraint on spread_time:
#{round(results2 %>% arrange(logloss) %>% dplyr::slice(1) %>% pull(logloss), 5)}"
#)

# Get results table and arrange by logloss
results %>% # Change to results2 if logloss better from above
  dplyr::arrange(logloss) %>%
  dplyr::select(eta, subsample, colsample_bytree, max_depth, logloss, min_child_weight, iter)

best_model <- results %>% # results2 as mentioned above
  dplyr::arrange(logloss) %>%
  dplyr::slice(1)

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

params

glue::glue("nrounds: {nrounds}")

# Train xgboost model using best parameters from above
xfg_model <- xgboost::xgboost(
  params = params,
  data = as.matrix(train_data),
  label = train_labels$label,
  nrounds = nrounds,
  verbose = 2
)

# Plot feature importance
importance <- xgboost::xgb.importance(
  feature_names = colnames(xfg_model),
  model = xfg_model
)
xgboost::xgb.ggplot.importance(importance_matrix = importance)

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

# Testing accuracy rather than logloss
MLmetrics::Accuracy(
  # say a team is predicted to win if they have win prob > .5
  preds %>%
    dplyr::mutate(pred = ifelse(xfg > .5, 1, 0)) %>%
    dplyr::pull(pred),
  # compare to whether they actually won
  preds$label
)

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
    title = "FG calibration plot"
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

saveRDS(best_model, "Models/xfg_xgboost_no_loc.rds")

saveRDS(xfg_model, glue::glue("Models/xfg noloc {cur_yr}.rds"))
