library(tidyverse)

ncaa_season <- 2023

# Loading things that are in the files
tourney_structure <- readRDS(glue::glue('March Madness Backup/tourney_structure_{ncaa_season}.rds'))

left_bracket <- c("South", "East")
right_bracket <- c("West", "Midwest")


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

teams <- readRDS(glue::glue('March Madness Backup/tourney_teams_{ncaa_season}.rds'))

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
