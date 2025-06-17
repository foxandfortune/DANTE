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
    filter(!is.na(result))
  
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
      away_seed, away_id, away_rest, away_dist, away_score,
      home_seed, home_id, home_rest, home_dist, home_score,
      result,
      winner_to
    ) %>% 
    as.data.frame()
  
  return(sched)
}

saveRDS(load_completed_games, 'Simulation Backup/Functions/load_completed_games.rds')
