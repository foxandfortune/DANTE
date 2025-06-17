library(hoopR)
library(sp)
library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
setwd('..')

# Add NOT IN function:
`%!in%` = Negate(`%in%`)

season <- 2023

load_completed_games <- function(season,
                                 left_bracket,
                                 right_bracket) {
  # Load full season using hoopR
  sched <- hoopR::load_mbb_schedule(seasons = {season}) %>% 
    # Filter for March Madness Tournament
    filter(tournament_id == 22) %>% 
    # Arrange by date
    arrange(date) %>%
    # Add result column
    mutate(result = home_score - away_score)
  
  # ONLY FOR TESTING
  sched <- sched %>% 
    filter(str_detect(notes_headline, "First Four") | str_detect(notes_headline, "1st Round") |
             str_detect(notes_headline, "2nd Round"))
  
  teams <- readRDS(glue::glue('March Madness Backup/tourney_teams_{season}.rds'))
  
  sched <- sched %>% 
    filter(!is.na(result)) %>% 
    left_join(teams %>% select(team_id, seed), by = c("away_id" = "team_id")) %>% 
    rename(away_seed = seed) %>% 
    left_join(teams %>% select(team_id, seed), by = c("home_id" = "team_id")) %>% 
    rename(home_seed = seed) %>% 
    mutate(region = case_when(
      str_detect(notes_headline, "South Region") ~ "South",
      str_detect(notes_headline, "West Region") ~ "West",
      str_detect(notes_headline, "Midwest Region") ~ "Midwest",
      str_detect(notes_headline, "East Region") ~ "East"),
      round_name = case_when(
        str_detect(notes_headline, "First Four") ~ "First Four",
        str_detect(notes_headline, "1st Round") ~ "First",
        str_detect(notes_headline, "2nd Round") ~ "Second",
        str_detect(notes_headline, "Sweet 16") ~ "Sweet 16",
        str_detect(notes_headline, "Elite 8") ~ "Elite 8",
        str_detect(notes_headline, "Final Four") ~ "Final 4",
        str_detect(notes_headline, "National Championship") ~ "Championship")) %>% 
    mutate(round_no = case_when(
      round_name == "First Four" ~ 0, round_name == "First" ~ 1,
      round_name == "Second" ~ 2, round_name == "Sweet 16" ~ 3,
      round_name == "Elite 8" ~ 4, round_name == "Final 4" ~ 5,
      round_name == "Championship" ~ 6)) %>% 
    mutate(away_dist = 0, home_dist = 0,
           away_rest = 0, home_rest = 0,
           winner_to = case_when(
             round_no == 0 ~ paste0(home_seed, " v ", {17 - home_seed}),
             round_no == 1 & home_seed %in% c(1, 8) ~ "32-1",
             round_no == 1 & home_seed %in% c(4, 5) ~ "32-2",
             round_no == 1 & home_seed %in% c(3, 6) ~ "32-3",
             round_no == 1 & home_seed %in% c(2, 7) ~ "32-4",
             round_no == 2 & home_seed %in% c(1, 16, 8, 9,
                                              5, 12, 4, 13) ~ "16-1",
             round_no == 2 & home_seed %in% c(6, 11, 3, 14,
                                              7, 10, 2, 15) ~ "16-2",
             round_no == 3 ~ "8-1",
             round_no == 4 & region %in% left_bracket ~ "4-1",
             round_no == 4 & region %in% right_bracket ~ "4-2",
             round_no == 5 ~ "Final")) %>% 
    select(
      region, round_name, round_no,
      away_seed, away_id, away_rest, away_dist,
      home_seed, home_id, home_rest, home_dist,
      result,
      winner_to
    ) %>% 
    as.data.frame()
  
  return(sched)
}

saveRDS(load_completed_games, 'Minos/Simulation Backup/Functions/load_completed_games.rds')

schedule <- load_completed_games(season = {cur_yr},
                     left_bracket = {left_bracket},
                     right_bracket = {right_bracket})

tourney_structure <- readRDS(glue::glue('Minos/March Madness Backup/tourney_structure_{season}.rds'))
teams <- readRDS(glue::glue('Minos/March Madness Backup/tourney_teams_{season}.rds'))

left_bracket <- c("South", "East")
right_bracket <- c("West", "Midwest")

# Load schedule filter tournament games --------------------------------
sched <- hoopR::load_mbb_schedule(seasons = {season})

## Filter tournament games
tourn <- sched %>% 
  filter(tournament_id == 22) %>% 
  arrange(date) %>% 
  mutate(result = home_score - away_score)

tourn_short <- tourn %>% 
  filter(str_detect(notes_headline, "First Four") | str_detect(notes_headline, "1st Round") |
         str_detect(notes_headline, "2nd Round") )

tourney_structure

# Create Tournament Structure ----------------
rounds_completed <- tourn_short %>% 
  filter(!is.na(result)) %>% 
  left_join(teams %>% select(team_id, seed), by = c("away_id" = "team_id")) %>% 
  rename(away_seed = seed) %>% 
  left_join(teams %>% select(team_id, seed), by = c("home_id" = "team_id")) %>% 
  rename(home_seed = seed) %>% 
  mutate(region = case_when(
    str_detect(notes_headline, "South Region") ~ "South",
    str_detect(notes_headline, "West Region") ~ "West",
    str_detect(notes_headline, "Midwest Region") ~ "Midwest",
    str_detect(notes_headline, "East Region") ~ "East"),
    round_name = case_when(
      str_detect(notes_headline, "First Four") ~ "First Four",
      str_detect(notes_headline, "1st Round") ~ "First",
      str_detect(notes_headline, "2nd Round") ~ "Second",
      str_detect(notes_headline, "Sweet 16") ~ "Sweet 16",
      str_detect(notes_headline, "Elite 8") ~ "Elite 8",
      str_detect(notes_headline, "Final Four") ~ "Final 4",
      str_detect(notes_headline, "National Championship") ~ "Championship")) %>% 
  mutate(round_no = case_when(
    round_name == "First Four" ~ 0, round_name == "First" ~ 1,
    round_name == "Second" ~ 2, round_name == "Sweet 16" ~ 3,
    round_name == "Elite 8" ~ 4, round_name == "Final 4" ~ 5,
    round_name == "Championship" ~ 6)) %>% 
  mutate(away_dist = 0, home_dist = 0,
         away_rest = 0, home_rest = 0,
         winner_to = case_when(
    round_no == 0 ~ paste0(home_seed, " v ", {17 - home_seed}),
    round_no == 1 & home_seed %in% c(1, 8) ~ "32-1",
    round_no == 1 & home_seed %in% c(4, 5) ~ "32-2",
    round_no == 1 & home_seed %in% c(3, 6) ~ "32-3",
    round_no == 1 & home_seed %in% c(2, 7) ~ "32-4",
    round_no == 2 & home_seed %in% c(1, 16, 8, 9,
                                     5, 12, 4, 13) ~ "16-1",
    round_no == 2 & home_seed %in% c(6, 11, 3, 14,
                                      7, 10, 2, 15) ~ "16-2",
    round_no == 3 ~ "8-1",
    round_no == 4 & region %in% left_bracket ~ "4-1",
    round_no == 4 & region %in% right_bracket ~ "4-2",
    round_no == 5 ~ "Final")) %>% 
  select(
    region, round_name, round_no,
    away_seed, away_id, away_rest, away_dist,
    home_seed, home_id, home_rest, home_dist,
    result,
    winner_to
  ) %>% 
  as.data.frame()
names(first_four_2023)

tourney_structure

first_rounds <- tourn_short %>% 
  filter(str_detect(notes_headline, "First Four") | str_detect(notes_headline, "1st Round")) %>%
  mutate(region = case_when(
    str_detect(notes_headline, "South Region") ~ "South",
    str_detect(notes_headline, "West Region") ~ "West",
    str_detect(notes_headline, "Midwest Region") ~ "Midwest",
    str_detect(notes_headline, "East Region") ~ "East"),
    round = case_when(
      str_detect(notes_headline, "First Four") ~ "First Four",
      str_detect(notes_headline, "1st Round") ~ "First"),
    round_no = case_when(
      round == "First Four" ~ 0,
      round == "First" ~ 1),
    away_dist = 0, home_dist = 0,
    home_rest = 0, away_rest = 0,
    result = home_score - away_score) %>% 
  select(region, round, round_no,
         away_seed = away_current_rank, away_id, away_rest, away_dist,
         home_seed = home_current_rank, home_id, home_rest, home_dist,
         result) %>% 
  mutate(winner_to = case_when(
    round_no == 0 & home_seed == 16 ~ "16 v 1",
    round_no == 0 & home_seed == 11 ~ "11 v 6",
    round_no == 1 & home_seed %in% c(1, 8) ~ "32-1",
    round_no == 1 & home_seed %in% c(4, 5) ~ "32-2",
    round_no == 1 & home_seed %in% c(3, 6) ~ "32-3",
    round_no == 1 & home_seed %in% c(2, 7) ~ "32-4"
  ))

for(rounds in rounds_completed) {
  
}



tourney_structure
schedule <- readRDS(glue::glue('March Madness Backup/first_four_{ncaa_season}.rds')) %>%
  select(
    region, round, round_no,
    away_seed, away_id, away_rest, away_dist,
    home_seed, home_id, home_rest, home_dist,
    result,
    winner_to
  )
