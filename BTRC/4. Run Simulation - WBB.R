library(tidyverse)
library(gsubfn)

# Set working directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd('..')

# Add NOT IN function:
`%!in%` = Negate(`%in%`)

# Set reference year
cur_yr <- 2026

# Sim method ------------------------
sim_method <- "to_oreb_ast"

# Load ratings ------------------
ratings <- readRDS(glue::glue("BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_wbb_{cur_yr}.rds"))
ratings_no_prior <- readRDS(glue::glue("BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_no_prior_wbb_{cur_yr}.rds"))

ratings <- list(
  pace = ratings$pace,
  ast = ratings$ast,
  oreb = ratings$oreb,
  to = ratings$to,
  rtg = ratings$rtg,
  
  disp_Rtg = 13
)

ratings_no_prior <- list(
  pace = ratings_no_prior$pace,
  ast = ratings_no_prior$ast,
  oreb = ratings_no_prior$oreb,
  to = ratings_no_prior$to,
  rtg = ratings_no_prior$rtg,
  
  disp_Rtg = 13
)

# Load teams -------------------
teams <- readRDS(glue::glue('_Helper Files/Team Data/team_database_wbb.rds')) %>% 
  mutate(team_id = as.integer(team_id))

# Load functions ----------------------------------------------------------
double_games <- readRDS("_Helper Files/Simulation Functions/double_games_wk.rds")
compute_league_ranks <- readRDS("_Helper Files/Simulation Functions/compute_league_ranks.rds")
simulate_round <- readRDS("_Helper Files/Simulation Functions/simulate_round_wk wbb.rds")
simulate_ncaa <- readRDS("_Helper Files/Simulation Functions/simulate_ncaa_wk.rds")
report <- readRDS("_Helper Files/Simulation Functions/report.rds")
is_single_digit_numeric <- readRDS("_Helper Files/Simulation Functions/is_single_digit_numeric.rds")
is_sequential <- readRDS("_Helper Files/Simulation Functions/is_sequential.rds")

# Load process_games and ratings based on sim method -------------------------------------------
process_games <- readRDS(glue::glue("_Helper Files/Simulation Functions/process_games_wk_{sim_method}.rds"))

# Load schedule -----------------
schedule <- wehoop::load_wbb_schedule(seasons = {cur_yr}) %>% 
  filter(status_type_completed == FALSE,
         home_id %in% teams$team_id & away_id %in% teams$team_id) %>%
  filter(game_date >= Sys.Date(),
         game_date <= Sys.Date() + 0)

unique(schedule$game_date)

## Adjusted schedule -----------
adj_schedule <- readRDS(glue::glue("BTRC/Stats/Season Schedules/schedule_adj_wbb_{cur_yr}.rds"))

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

rating_np_ids <- ratings_no_prior$rtg %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, "team_id_")) %>% 
  pull(team_id)

schedule <- schedule %>% 
  filter(home_id %in% rating_ids & away_id %in% rating_ids) %>% 
  filter(!is.na(away_rest))

schedule_np <- schedule %>% 
  filter(home_id %in% rating_np_ids & away_id %in% rating_np_ids) %>% 
  filter(!is.na(away_rest))

print(length(schedule$game_id))

##########################RUN SIMULATION #########################################################################
## Set seed ------------------
set.seed(421)

## Run it -------------------------------------
object <- simulate_ncaa(ncaa_season =  {cur_yr},
                     process_games = {process_games},
                     schedule = {schedule},
                     if_ended_today = FALSE,
                     fresh_season = TRUE,
                     ratings = {ratings},
                     simulations = 10000,
                     sim_include = "REG")

## Set seed ------------------
set.seed(421)

## Run again without priors -------------------------------------
object_no_prior <- simulate_ncaa(ncaa_season =  {cur_yr},
                                 process_games = {process_games},
                                 schedule = {schedule_np},
                                 if_ended_today = FALSE,
                                 fresh_season = TRUE,
                                 ratings = {ratings_no_prior},
                                 simulations = 10000,
                                 sim_include = "REG")

##########################GET ODDS############################################################################
library(jsonlite)

## Load odds functions ---------
get_odds <- readRDS('_Helper Files/Simulation Functions/get_odds.rds')
double_odds <- readRDS('_Helper Files/Other/double_odds_wk.rds')
game_ids <- schedule %>% 
  select(game_id, game_date, team_id = home_id) %>% 
  bind_rows(schedule %>% 
              select(game_id, game_date, team_id = away_id))

## Get game dates --------
dates <- schedule %>% 
  select(game_date) %>% 
  distinct()

# Combine all bets --------------------------------------
all_results <- as.data.frame(object$games) %>% 
  double_odds() %>% 
  left_join(schedule %>% 
              select(game_date, team_id = home_id, opp_id = away_id) %>% 
              bind_rows(schedule %>% 
                          select(game_date, team_id = away_id, opp_id = home_id)),
            by = c("team_id", "opp_id")) %>% 
  mutate(opp_score = team_score - result,
         game_total = team_score + opp_score) %>% 
  left_join(teams %>% 
              select(team_id, Team = team), by = "team_id") %>%
  left_join(teams %>% 
              select(opp_id = team_id, Opp = team), by = "opp_id") %>% 
  with_groups(.groups = c(team_id, Team,
                          opp_id, Opp, game_date),
              summarise, win = mean(outcome),
              result = -median(result),
              team_total = round(mean(team_score)/.5, 0) * 0.5,
              opp_total = round(mean(opp_score)/.5, 0) * 0.5,
              game_total = round(mean(game_total)/.5, 0) * 0.5) %>% 
  left_join(schedule %>% 
              select(game_date, team_id = home_id, game_id) %>% 
              bind_rows(schedule %>% 
                          select(game_date, team_id = away_id, game_id)),
            by = c("team_id", "game_date")) %>% 
  select(-game_date) %>% 
  left_join(wehoop::load_wbb_schedule() %>% 
              select(game_id, game_date = game_date_time), by = "game_id") %>% 
  relocate(c(game_date, game_date, game_id)) %>% 
  arrange(game_date, game_id)

all_results_no_prior <- as.data.frame(object_no_prior$games) %>% 
  double_odds() %>% 
  left_join(schedule %>% 
              select(game_date, team_id = home_id, opp_id = away_id) %>% 
              bind_rows(schedule %>% 
                          select(game_date, team_id = away_id, opp_id = home_id)),
            by = c("team_id", "opp_id")) %>% 
  mutate(opp_score = team_score - result,
         game_total = team_score + opp_score) %>% 
  left_join(teams %>% 
              select(team_id, Team = team), by = "team_id") %>%
  left_join(teams %>% 
              select(opp_id = team_id, Opp = team), by = "opp_id") %>% 
  with_groups(.groups = c(team_id, Team,
                          opp_id, Opp, game_date),
              summarise, win = mean(outcome),
              result = -median(result),
              team_total = round(mean(team_score)/.5, 0) * 0.5,
              opp_total = round(mean(opp_score)/.5, 0) * 0.5,
              game_total = round(mean(game_total)/.5, 0) * 0.5) %>% 
  left_join(schedule %>% 
              select(game_date, team_id = home_id, game_id) %>% 
              bind_rows(schedule %>% 
                          select(game_date, team_id = away_id, game_id)),
            by = c("team_id", "game_date")) %>% 
  select(-game_date) %>% 
  left_join(wehoop::load_wbb_schedule() %>% 
              select(game_id, game_date = game_date_time), by = "game_id") %>% 
  relocate(c(game_date, game_date, game_id)) %>% 
  arrange(game_date, game_id)

## Combine results with no prior
all_results <- all_results %>% 
  left_join(all_results_no_prior %>% 
              select(game_id, team_id,
                     win_noprior = win,
                     result_noprior = result), by = c("game_id", "team_id")) %>% 
  relocate(win_noprior, .after = win) %>% 
  relocate(result_noprior, .after = result)

## Save -----
saveRDS(all_results,
        glue::glue('BTRC/Stats/Season Schedules/Results/{cur_yr}/all_results_{max(schedule$game_date)}.rds'))
