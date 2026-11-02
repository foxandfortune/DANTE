library(tidyverse)
library(gsubfn)

# Add NOT IN function:
`%!in%` = Negate(`%in%`)

# Set reference year and load the simulation functions
source("_Helper Files/season.R")
source("_Helper Files/Function Scripts/sim_functions.R")
cur_yr <- dante_season

# Sim method ------------------------
# "calibrated" = calibrated engine: phase-specific margin scale and sd, separate
# total sd (correlated scoring noise) and a home-court correction, read from
# ratings$calibration (built by _Backtest/build_calibration.R).
# Set to "to_oreb_ast" to use the previous engine (x1.3 stretch + disp_Rtg).
sim_method <- "calibrated"
process_games <- switch(sim_method,
                        calibrated  = process_games_calibrated,
                        to_oreb_ast = process_games_to_oreb_ast)

# Load ratings ------------------
# In-season ratings, or the preseason priors before any games have been played.
# The no-prior ratings only exist once games have been played (NULL until then).
ratings <- load_sim_ratings("mbb", cur_yr)
ratings_no_prior <- load_sim_ratings("mbb", cur_yr, no_prior = TRUE)

# Load teams -------------------
teams <- readRDS('_Helper Files/Team Data/team_database.rds') %>%
  mutate(team_id = as.integer(team_id))

# Load schedule -----------------
full_schedule <- hoopR::load_mbb_schedule(seasons = cur_yr)

schedule <- full_schedule %>%
  filter(status_type_completed == FALSE,
         home_id %in% teams$team_id & away_id %in% teams$team_id) %>%
  filter(game_date >= Sys.Date(),
         game_date <= Sys.Date() + 0)

unique(schedule$game_date)

## Adjusted schedule (rest and travel, from script 3) -----------
adj_schedule <- readRDS(glue::glue("VRGL/Stats/Season Schedules/schedule_adj_{cur_yr}.rds"))

## Add travel/rest to schedule ---------
schedule <- schedule %>%
  left_join(adj_schedule, by = c('game_date', 'game_id', 'home_id', 'away_id')) %>%
  mutate(result = home_score - away_score) %>%
  select(game_date, game_id,
         away_id, away_rest, away_dist, away_score,
         home_id, home_rest, home_dist, home_score,
         is_neutral = neutral_site,
         result)

# Check for errors
schedule %>%
  filter(is.na(home_rest) | is.na(away_rest) | is.na(home_dist) | is.na(away_dist))

rating_ids <- ratings$rtg %>%
  filter(str_detect(name, "team_id_")) %>%
  mutate(team_id = str_remove_all(name, "team_id_")) %>%
  pull(team_id)

schedule <- schedule %>%
  filter(home_id %in% rating_ids & away_id %in% rating_ids) %>%
  filter(!is.na(away_rest))

# Games where both teams have no-prior ratings (new teams can be missing early on)
if (!is.null(ratings_no_prior)) {
  rating_np_ids <- ratings_no_prior$rtg %>% 
    filter(str_detect(name, "team_id_")) %>% 
    mutate(team_id = str_remove_all(name, "team_id_")) %>% 
    pull(team_id)
  
  schedule_np <- schedule %>%
    filter(home_id %in% rating_np_ids & away_id %in% rating_np_ids)
}

print(length(schedule$game_id))

# Exit gracefully if no games to simulate today
if (nrow(schedule) == 0) {
  message("No MBB games to simulate today. Exiting.")
  if (interactive()) invokeRestart("abort") else quit(save = "no", status = 0)
}

##########################RUN SIMULATION #########################################################################
## Set seed ------------------
set.seed(421)

## Run it -------------------------------------
object <- simulate_ncaa_week(ncaa_season = cur_yr,
                             process_games = process_games,
                             schedule = schedule,
                             if_ended_today = FALSE,
                             fresh_season = TRUE,
                             ratings = ratings,
                             simulations = 10000,
                             sim_include = "REG",
                             team_file = '_Helper Files/Team Data/team_database.rds')

## Run again without priors (once games have been played) -------------------------------------
object_no_prior <- NULL
if (!is.null(ratings_no_prior) && nrow(schedule_np) > 0) {
  set.seed(421)
  object_no_prior <- simulate_ncaa_week(ncaa_season = cur_yr,
                                        process_games = process_games,
                                        schedule = schedule_np,
                                        if_ended_today = FALSE,
                                        fresh_season = TRUE,
                                        ratings = ratings_no_prior,
                                        simulations = 10000,
                                        sim_include = "REG",
                                        team_file = '_Helper Files/Team Data/team_database.rds')
}

##########################GET ODDS AND BETS####################################################################
## Get odds (a failed odds pull skips the bets but keeps the results) ----------
dates <- schedule %>%
  select(game_date) %>%
  distinct()

odds <- tryCatch(get_odds(dates,
                          bk_name = "Consensus",
                          league_nm = 'ncaab'),
                 error = function(e) {
                   message("Couldn't get odds: ", conditionMessage(e))
                   NULL
                 })

all_bets <- build_bets(object, odds, schedule)

##########################RESULTS##############################################################################
game_times <- full_schedule %>%
  select(game_id, game_date = game_date_time)

all_results <- summarise_sim_results(object, schedule, teams, game_times)

all_results_no_prior <- if (is.null(object_no_prior)) NULL else
  summarise_sim_results(object_no_prior, schedule, teams, game_times)

## Combine results with no prior
all_results <- add_no_prior_results(all_results, all_results_no_prior)

## Save -----
results_dir <- glue::glue('VRGL/Stats/Season Schedules/Results/{cur_yr}')
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

if (!is.null(all_bets)) {
  saveRDS(all_bets,
          glue::glue('{results_dir}/all_bets_{max(schedule$game_date)}.rds'))
}

saveRDS(all_results,
        glue::glue('{results_dir}/all_results_{max(schedule$game_date)}.rds'))
