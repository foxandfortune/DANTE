library(wehoop)
library(tidyverse)

`%!in%` = Negate(`%in%`)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set season
cur_yr <- 2024

# Load full pbp and raw pbp, roster and team data ----------------------------------------------------
pbp_raw <- wehoop::load_wbb_pbp(seasons = c({cur_yr},
                                            {cur_yr-1},
                                            {cur_yr-2},
                                            {cur_yr-3},
                                            {cur_yr-4}))

# Load roster
roster <- wehoop::load_wbb_player_box(seasons = c({cur_yr},
                                                  {cur_yr-1},
                                                  {cur_yr-2},
                                                  {cur_yr-3},
                                                  {cur_yr-4}))

# Check for missing play types ------------
playtypes <- pbp_raw %>% 
  mutate(type_id = as.character(type_id)) %>% 
  select(type_id, type_text) %>% 
  unique() %>% 
  arrange(type_id)

playtypes %>% 
  print(n = Inf)

pbp_raw <- pbp_raw %>% filter(type_id != 0) %>% 
  mutate(type_id = case_when(
    type_id == 20437 ~ 437,
    type_id == 20558 ~ 558,
    type_id == 20572 ~ 572,
    type_id == 20574 ~ 574,
    type_id == 30558 ~ 558,
    TRUE ~ type_id))

# Get unique players and teams with positions -----------------------------------------
players <- roster %>% 
  select(athlete_id, athlete_display_name, athlete_jersey,
         athlete_headshot_href, athlete_position_abbreviation, team_id) %>% 
  unique()

doubles <- players %>% 
  with_groups(.groups = athlete_id, mutate, n = n()) %>% 
  filter(n > 1) %>% 
  arrange(athlete_id, athlete_headshot_href) %>% 
  with_groups(.groups = athlete_id, mutate, row = row_number()) %>% 
  filter(row == 1) %>% 
  select(-c(n, row)) %>% 
  as.data.frame()

players2 <- as.data.frame(players) %>% 
  filter(athlete_id %!in% doubles$athlete_id) %>% 
  rbind(doubles)

pbp_raw <- pbp_raw %>% 
  mutate(clock_minutes = as.numeric(clock_minutes), # Fix the seconds remaining thing
         clock_seconds = as.numeric(clock_seconds),
         secs_remaining = case_when( 
           period_display_value == "1st Half" ~ (20 * 60) + (60 * clock_minutes) + clock_seconds,
           period_display_value == "2nd Half" ~ (60 * clock_minutes) + clock_seconds,
           period_display_value == "1st Quarter" ~ (30 * 60) + (60 * clock_minutes) + clock_seconds,
           period_display_value == "2nd Quarter" ~ (20 * 60) + (60 * clock_minutes) + clock_seconds,
           period_display_value == "3rd Quarter" ~ (10 * 60) + (60 * clock_minutes) + clock_seconds,
           period_display_value == "4th Quarter" ~ (60 * clock_minutes) + clock_seconds,
           str_detect(period_display_value, "OT") ~ (60 * clock_minutes) + clock_seconds
         )) %>%
  # Get rid of a bunch of garbage stuff
  select(-c(start_game_seconds_remaining, #start_quarter_seconds_remaining, start_half_seconds_remaining,
            #end_quarter_seconds_remaining, end_half_seconds_remaining, end_game_seconds_remaining,
            coordinate_x, coordinate_y)) %>% 
  # Add players 
  left_join(players2, by = c("athlete_id_1" = "athlete_id",
                             "team_id")) %>% 
  # Rename some of the column titles 
  rename(player = athlete_display_name,
         number = athlete_jersey,
         player_id = athlete_id_1,
         shot_value = score_value,
         other_player_id = athlete_id_2,
         pos = athlete_position_abbreviation,
         x = coordinate_x_raw,
         y = coordinate_y_raw) %>% 
  # Also move some stuff
  relocate(away_score, .before = home_score) %>% 
  relocate(game_id, .before = shooting_play) %>% 
  # Add some columns
  mutate(is_home = case_when(
    team_id == home_team_id ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  with_groups(.groups = c(game_id, period_display_value), mutate, # Elapsed time for shot clock
              elapsed = lag(secs_remaining) - secs_remaining) %>% 
  mutate(elapsed = replace_na(elapsed, 0)) %>% 
  # Point value for actual scoring plays; zero for misses
  mutate(point_value = case_when(
    scoring_play == TRUE ~ shot_value,
    scoring_play == FALSE ~ as.integer(0)
  )) %>%
  # Time to add possessions
  mutate(end_possession = case_when(
    type_id == "540" & scoring_play == TRUE & lead(type_id) != "540" ~ TRUE,
    type_id == "540" & lead(type_id) == "540" ~ FALSE,
    type_id == "540" ~ FALSE,
    scoring_play == TRUE & lead(type_id) == "519" & clock_display_value == lead(clock_display_value) ~ FALSE,
    lead(type_id) == "586" ~ FALSE,
    lead(type_id) == "587" ~ TRUE,
    type_id == "598" ~ TRUE,
    scoring_play == TRUE ~ TRUE,
    type_id == "412" ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  mutate(poss_tm = case_when(
    lag(end_possession) == TRUE ~ team_id,
    shooting_play == TRUE ~ team_id,
    type_id %in% c("615", "587") ~ team_id,
    NULL)) %>% 
  fill(poss_tm, .direction = "down") %>% # Fill down
  mutate(poss_tm = case_when(
    lag(end_possession) == TRUE & type_id == "519" ~ lead(poss_tm),
    TRUE ~ poss_tm
  )) %>% 
  mutate(poss_tm = case_when(
    type_id == "449" ~ lag(poss_tm),
    TRUE ~ poss_tm
  )) %>% 
  mutate(poss_tm = case_when(
    type_id == "586" ~ lag(poss_tm),
    type_id == "587" ~ team_id,
    TRUE ~ poss_tm
  )) %>%
  # Add possession ID
  with_groups(.groups = game_id, mutate, poss_id = data.table::rleid(poss_tm)) %>% 
  # Total elapsed time for offensive rebounds
  with_groups(.groups = c(game_id, poss_id), mutate, poss_time = cumsum(elapsed)) %>% 
  with_groups(.groups = c(game_id, poss_id), mutate, poss_row = row_number()) %>%
  # Add possession start types
  mutate(poss_start = case_when(
    poss_row == 1 & type_id %in% c("587", "615", "607", "449") ~ type_text,
    poss_row == 1 & lag(type_id) == "412" ~ "NewPeriod",
    poss_row == 1 & lag(scoring_play) == TRUE ~ "MadeBasket",
    poss_row == 1 & lag(type_id) %in% c("578", "579", "580") ~ "Timeout",
    poss_row == 1 & lag(type_id) %in% c("521") ~ "TechFoul",
    poss_row == 1 & lag(type_id) == "519" ~ "OffFoul",
    poss_row == 1 & lag(type_id) == "598" ~ "Turnover",
    poss_row == 1 & game_play_number == 1 ~ "Jumpball",
    poss_row == 1 & lag(type_id) == "449" ~ lag(type_text),
    poss_row == 1 ~ "MadeBasket"
  )) %>% 
  fill(poss_start, .direction = "down") %>% # Fill down
  # Add shot clock
  mutate(shot_clock = case_when(
    shooting_play == TRUE & lag(type_id) == "586" & lag(type_id, n = 2) == "618" ~ 30 - poss_time,
    shooting_play == TRUE & lag(type_id) == "586" & season < 2020 ~ 30 - poss_time,
    shooting_play == TRUE & lag(type_id) == "586" & season >= 2020 ~ 20 - poss_time,
    shooting_play == TRUE & game_play_number == 1 ~ 30 - (2400 - secs_remaining),
    TRUE ~ 30 - elapsed),
    score_diff = home_score - away_score,
    shot_diff = case_when( # Score different at shot
      shooting_play == TRUE & is_home == TRUE ~ score_diff - point_value,
      shooting_play == TRUE & is_home == FALSE ~ score_diff + point_value,
      TRUE ~ score_diff),
    shot_dist = sqrt(abs(x - 25)^2 + abs(y - 5)^2), # Shot distance
    shot_angle = if_else(y <= 5, 90, # Shot angle
                         if_else(x == 25,
                                 0, (180/pi)*atan(abs(x - 25)/(y-5)))),
    is_atrim = case_when( # At rim or not
      shot_dist <= 3 ~ TRUE,
      str_detect(text, "Layup") ~ TRUE,
      str_detect(text, "Dunk") ~ TRUE,
      TRUE ~ FALSE),
    is_fastbreak = case_when( # Based on shot clock of all turnovers
      poss_start %in% c("Defensive Rebound", "Steal", "Turnover") & shot_clock >= 25 ~ TRUE,
      TRUE ~ FALSE
    ), is_putback = case_when( # Add "is putback"
      shooting_play == TRUE & is_atrim == TRUE & lag(type_id) == "586" & elapsed <= 2 ~ TRUE,
      TRUE ~ FALSE
    ), oreb_type = case_when( # Offensive rebound type (putback/other)
      type_id == "586" & lead(is_putback) == TRUE ~ "rim_Oreb",
      type_id == "586" ~ "other_Oreb"
    ), fouled_on_shot = case_when( # Fouled on shot
      shooting_play == TRUE & lead(type_id) == "519" & lead(elapsed) == 0 ~ TRUE,
      shooting_play == TRUE ~ FALSE
    )) %>% 
  relocate(c(poss_tm, poss_id, end_possession, poss_row, poss_start, elapsed, shot_clock), .after = team_id)

# Fix shot clock below 0; use possession start, exclude fast break
mean_sc <- pbp_raw %>%
  filter(shot_clock >= 0, shooting_play == TRUE, is_fastbreak == FALSE) %>% 
  with_groups(.groups = c(season, poss_start), summarise, mean_sc = mean(shot_clock))

mean_sc

# Add mean shot_clock for values less than 0
pbp_raw <- pbp_raw %>% 
  left_join(mean_sc, by = c("season", "poss_start")) %>% 
  mutate(shot_clock = case_when(
    shot_clock >= 0 ~ shot_clock,
    shooting_play == TRUE & shot_clock < 0 ~ mean_sc, 
    TRUE ~ shot_clock # Don't care about non shots tbh
  )) %>%
  select(-mean_sc)

pbp_raw %>% 
  filter(shooting_play == TRUE, shot_clock < 0)

## Save file for model ----------------
saveRDS(pbp_raw, 'Shot Files/pbp_raw_current.rds')
