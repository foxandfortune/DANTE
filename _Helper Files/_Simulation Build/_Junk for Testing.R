library(tidyverse)
library(gsubfn)

# Add NOT IN function:
`%!in%` = Negate(`%in%`)

# Set reference year
cur_yr <- 2025

# Sim method ------------------------
sim_method <- "to_reb_ast"

# Load ratings ------------------
ratings <- readRDS(glue::glue("_Helper Files/inseason_ratings_all_{cur_yr}.rds"))

ratings <- list(
  pace = ratings$pace,
  ast = ratings$ast,
  oreb = ratings$oreb,
  to = ratings$to,
  rtg = ratings$rtg,
  
  disp_Rtg = 13
)

# Load functions ----------------------------------------------------------
double_games <- readRDS("_Helper Files/Simulation Functions/double_games.rds")
compute_conference_seeds <- readRDS("_Helper Files/Simulation Functions/compute_conference_seeds.rds")
simulate_round <- readRDS("_Helper Files/Simulation Functions/simulate_round.rds")
simulate_ncaa <- readRDS("_Helper Files/Simulation Functions/simulate_ncaa.rds")
report <- readRDS("_Helper Files/Simulation Functions/report.rds")
is_single_digit_numeric <- readRDS("_Helper Files/Simulation Functions/is_single_digit_numeric.rds")
is_sequential <- readRDS("_Helper Files/Simulation Functions/is_sequential.rds")
load_completed_games <- readRDS("_Helper Files/Simulation Functions/load_completed_games.rds")

# Load process_games and ratings based on sim method -------------------------------------------
process_games <- readRDS(glue::glue("_Helper Files/Simulation Functions/process_games_{sim_method}.rds"))

# Other setup ----------------------------------------------------------------------------------
ncaa_season <- 2025
season <- ncaa_season
mbb_wbb <- 'MBB'

unique(sched$notes_headline)

# Loading things that are in the files
tourney_structure <- readRDS(glue::glue('_Helper Files/MM - Tourney and First Fours/MBB/tourney_structure_{ncaa_season}.rds'))

left_bracket <- c("South", "West")
right_bracket <- c("East", "Midwest")
round_num <- 6
simulations <- 10
unique(sched$notes_headline)
iter_sims <- 5
played_round <- 0
test_round <- NULL
games %>% 
  filter(round_no == 4)
simulate_week(teams = teams,
              games = games,
              round_num = round_num)

return_value <- process_games(teams = teams,
                              games = games,
                              round_num = round_num,
                              ratings = ratings)

games <- games %>%
  filter(round_no != 6)

return_value$games %>% 
  filter(round_no == max(round_no))

schedule <- load_completed_games(season = ncaa_season,
                                 left_bracket = {left_bracket},
                                 right_bracket = {right_bracket}) %>%
  select(
    region, round_name, round_no,
    away_seed, away_id, away_rest, away_dist,
    home_seed, home_id, home_rest, home_dist,
    result,
    winner_to
  )

object$overall

teams <- readRDS(glue::glue('_Helper Files/MM - Tourney and First Fours/MBB/tourney_teams_{ncaa_season}.rds'))



# Add sim numbers to data frames
teams <- teams %>% 
  mutate(sim = 1)

games <- schedule %>% 
  mutate(sim = 1) %>% 
  select(sim, everything())

played_round <- 2

compute_conference_seeds(teams = teams,
                         games = games)



# For quickly adding dummy results to games
round_num <- 1

games <- games %>% 
  with_groups(.groups = c(away_id), mutate,
              result = case_when(
                is.na(result) & round_no == round_num ~ round(rnorm(1, mean = 0, sd = 5), 0),
                TRUE ~ result)) %>% 
  mutate(result = ifelse(result == 0, 1, result))


sim <- simulate_ncaa(ncaa_season =  2023,
                     process_games = {process_games},
                     playoff_seeds = 17,
                     left_bracket = c("South", "East"),
                     right_bracket = c("West", "Midwest"),
                     if_ended_today = FALSE,
                     fresh_tourney = TRUE,
                     ratings = {ratings},
                     simulations = 10,
                     sim_include = "POST")

rm(games)
