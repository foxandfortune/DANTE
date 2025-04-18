library(hoopR)
library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set season
cur_yr <- 2025

`%!in%` = Negate(`%in%`)

# Load full pbp and raw pbp, roster and team data ----------------------------------------------------
pbp_full <- readRDS(glue::glue("Stats/PBP and Shot Data/pbp_{cur_yr}.rds"))

pbp_raw <- hoopR::load_mbb_pbp(seasons = {cur_yr}) #%>% 
  #mutate(type_id = case_when(
    #type_id == 20437 ~ 437,
    #type_id == 20558 ~ 558,
    #type_id == 20572 ~ 572,
    #type_id == 20574 ~ 574,
    #type_id == 30558 ~ 558,
    #TRUE ~ type_id))

#unique(pbp_raw$game_date)

pbp_raw <- pbp_raw %>% 
  filter(game_id %!in% pbp_full$game_id)

print(length(pbp_raw$game_id))

Sys.sleep(3)

# Load roster
roster <- hoopR::load_mbb_player_box(seasons = {cur_yr})

# Check for missing play types ------------
playtypes <- pbp_raw %>% 
  mutate(type_id = as.character(type_id)) %>% 
  select(type_id, type_text) %>% 
  unique() %>% 
  arrange(type_id)

playtypes %>% 
  print(n = Inf)

pbp_raw <- pbp_raw %>% filter(type_id != 0)

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
    shooting_play == TRUE & lag(type_id) == "586" ~ 20 - poss_time,
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
  bind_rows(pbp_full) %>% 
  filter(shot_clock >= 0, shooting_play == TRUE, is_fastbreak == FALSE) %>% 
  with_groups(.groups = poss_start, summarise, mean_sc = mean(shot_clock))

mean_sc

# Add mean shot_clock for values less than 0
pbp_raw <- pbp_raw %>% 
  left_join(mean_sc, by = "poss_start") %>% 
  mutate(shot_clock = case_when(
    shot_clock >= 0 ~ shot_clock,
    shooting_play == TRUE & shot_clock < 0 ~ mean_sc, 
    TRUE ~ shot_clock # Don't care about non shots tbh
  )) %>%
  select(-mean_sc)

pbp_raw %>% 
  filter(shooting_play == TRUE, shot_clock < 0)

# Save roster ---------------
saveRDS(roster, glue::glue("Stats/Rosters/roster_{cur_yr}.rds"))

# Add expected values to shots --------------------------------------------
## Shots with location -----------
shots_loc <- pbp_raw %>% 
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

# Shots without location
shots_noloc <- pbp_raw %>% 
  filter(shooting_play == TRUE, type_id != "540", is.na(shot_dist)) %>% 
  mutate(pos = case_when(
    pos != "NA" & !is.na(pos) ~ pos,
    is.na(pos) ~ "ATH",
    pos == "NA" ~ "ATH"
  )) %>% 
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

# Free throws
shots_ft <- pbp_raw %>% 
  filter(shooting_play == TRUE, type_id == "540") %>% 
  mutate(pos_group = case_when(
    pos %in% c("C", "G", "F", "ATH") ~ pos,
    pos %in% c("SF", "PF") ~ "F",
    pos %in% c("PG", "SG") ~ "G",
    is.na(pos) | pos == "NA" ~ "ATH"
  ))

# Check to make sure all shots are included
sum(pbp_raw$shooting_play) == sum(length(shots_ft$game_play_number),
                                  length(shots_loc$game_play_number),
                                  length(shots_noloc$game_play_number))

# Add preds to shots --------------------------------------------------------------------------------------
## Load packages ------
library(fastDummies)
library(splitTools)
library(dials)
library(xgboost)

# Models
xfg_loc <- xgb.load(glue::glue("Stats/Models/xfg {cur_yr}.model"))

xfg_noloc <- xgb.load(glue::glue("Stats/Models/xfg noloc {cur_yr}.model"))

xfg_ft <- readRDS(glue::glue("Stats/Models/ft {cur_yr}.rds"))

# Preds to location shots
set.seed(421)

preds_loc <- stats::predict(
  xfg_loc,
  # get rid of the things not needed for prediction here
  as.matrix(shots_loc %>% select(-c(label, game_id, game_play_number,
                                    period_display_value, period_number,
                                    team_id, poss_tm, poss_id, poss_row, poss_start,
                                    player_id, other_player_id, player, number, athlete_headshot_href,
                                    away_team_id, home_team_id,
                                    point_value, shot_value)
  ))
) %>%
  tibble::as_tibble() %>%
  dplyr::rename(xfg = value) %>%
  dplyr::bind_cols(shots_loc)

# Preds to no-location shots
set.seed(421)

preds_noloc <- stats::predict(
  xfg_noloc,
  # get rid of the things not needed for prediction here
  as.matrix(shots_noloc %>% select(-c(label, game_id, game_play_number,
                                      period_display_value, period_number,
                                      team_id, poss_tm, poss_id, poss_row, poss_start,
                                      player_id, other_player_id, player, number, athlete_headshot_href,
                                      away_team_id, home_team_id,
                                      point_value, shot_value)
  ))
) %>%
  tibble::as_tibble() %>%
  dplyr::rename(xfg = value) %>%
  dplyr::bind_cols(shots_noloc)

# Preds to free throws
preds_ft <- shots_ft %>% 
  mutate(label = as.numeric(scoring_play)) %>% 
  left_join(xfg_ft, by = c("pos_group" = "pos")) %>% 
  rename(xfg = xFG)

# Check the metrics
MLmetrics::LogLoss(preds_loc$xfg, preds_loc$label)
MLmetrics::LogLoss(preds_noloc$xfg, preds_noloc$label)
MLmetrics::LogLoss(preds_ft$xfg, preds_ft$label)

# Add xfg numbers to play-by-play -------------------------------------------------------------
all_preds <- rbind(preds_loc %>% 
                     select(game_id, game_play_number, xfg),
                   preds_noloc %>% 
                     select(game_id, game_play_number, xfg),
                   preds_ft %>% 
                     select(game_id, game_play_number, xfg))

# Bind xfg to pbp
pbp_raw <- pbp_raw %>% 
  left_join(all_preds, by = c("game_id", "game_play_number"))

# Add possession values and other additional stats
pbp_raw <- pbp_raw %>% 
  mutate(xfg = replace_na(xfg, 0),
         xfg_noft = case_when(
           type_id == "540" ~ 0,
           type_id != "540" ~ xfg),
         is_fg = case_when(
           shooting_play == TRUE & type_id != "540" ~ TRUE,
           TRUE ~ FALSE)) %>% 
  # Add expected shot value and points above expectation
  mutate(xpts = xfg * shot_value,
         pax = point_value - xpts,
         is_oreb = case_when( # For counting offensive rebounds
           type_id == "586" ~ 1,
           TRUE ~ 0)) %>% 
  with_groups(.groups = c(game_id, poss_id), mutate,
              cum_shots = cumsum(is_fg), # Cumulative non-FT shots per possession
              cum_xfg = cumsum(xfg_noft) - xfg_noft, # For adjusting secondary shots
              first_xfg = case_when( # For xfg of offensive rebounded shots
                shooting_play == TRUE & type_id != "540" & cum_shots == 1 ~ xfg)) %>% 
  fill(first_xfg, .direction = "down") %>% 
  with_groups(.groups = c(game_id, poss_id), mutate,
              xfg_adj = (1 - cum_xfg) * xfg, # Adjust for subsequent shots in possession
              xfg_oreb = (1 - cum_xfg + first_xfg) * xfg,
              xfg_adj_noft = case_when(
                type_id == "540" ~ 0,
                type_id != "540" ~ xfg_adj),
              xfg_adj_oreb_noft = case_when(
                type_id == "540" ~ 0,
                type_id != "540" ~ xfg_oreb),
              cum_xfg_adj = cumsum(xfg_adj_noft) - xfg_adj_noft,
              cum_xfg_oreb = cumsum(xfg_adj_oreb_noft) - xfg_adj_oreb_noft,
              readjust_xfg = (1 - cum_xfg_adj) * xfg,
              readjust_xfg_oreb = (1 - cum_xfg_oreb) * xfg) %>% # For offensive rebounds
  mutate(adj_xpts = readjust_xfg * shot_value, # Adjust expected points
         adj_oreb_xpts = readjust_xfg_oreb * shot_value) %>% # Adjust offensive rebound points
  mutate(adj_oreb_xpts = case_when(
    cum_shots == 1 ~ 0, # Exclude first shot expected points
    TRUE ~ adj_oreb_xpts))%>% 
  with_groups(.groups = c(game_id, poss_id), mutate,
              poss_xpts = sum(adj_xpts, na.rm = TRUE), # Total expected points for possessions
              poss_xpts_oreb = sum(adj_oreb_xpts, na.rm = TRUE), # Offensive rebound possession points
              poss_oreb = sum(is_oreb)) %>%  # Total offensive rebounds per possession
  select(-c(xfg_noft, cum_shots:adj_oreb_xpts))


# Add the new values back to the old ones ---------------------
pbp_full <- bind_rows(pbp_full,
                      pbp_raw)

saveRDS(pbp_full, glue::glue("Stats/PBP and Shot Data/pbp_{cur_yr}.rds"))

# Clear out extra objects ------------------------------
pbp <- pbp_full

rm(all_preds, doubles, mean_sc,
   pbp_full, pbp_raw, players, players2,
   playtypes, preds_ft, preds_loc, preds_noloc,
   roster, shots_ft, shots_loc, shots_noloc,
   xfg_ft, xfg_loc, xfg_noloc)

############################# Create summary files ####################
library(sp)

# Load teams -----
teams <- readRDS(glue::glue("Stats/Teams/team_database.rds"))

# Load schedule ------ 
schedule <- hoopR::load_mbb_schedule(seasons = {cur_yr}) %>% 
  as.data.frame() %>% 
  # Filter for teams in database
  filter(home_id %in% teams$team_id | away_id %in% teams$team_id) %>% 
  # Exclude canceled/postponed games
  filter(status_type_description %!in% c("Postponed", "Canceled"))

# Calculate days rest based on schedule ---------------
sched_doubled <- schedule %>% 
  select(game_id = id, game_date, team_id = home_id,
         venue_id, neutral_site, conference_competition) %>% 
  bind_rows(schedule %>% 
              select(game_id = id, game_date, team_id = away_id,
                     venue_id, neutral_site, conference_competition)) %>% 
  distinct() %>% 
  arrange(game_date) %>% 
  with_groups(.groups = c(team_id), mutate, days_rest = as.numeric(game_date - lag(game_date))) %>% 
  # Set NA values to 7 days; most for start of season games
  mutate(days_rest = replace_na(days_rest, 7))

# Get correct scores for games; filter out pbp which does not match to correct scores ------ 
## Load team box -----
team_box <- hoopR::load_mbb_team_box({cur_yr}) %>% 
  mutate(efg_adj = (0.5 * free_throws_made + field_goals_made + 0.5 * three_point_field_goals_made) / 
           (field_goals_attempted + free_throws_attempted),
         ast_rt = assists / field_goals_made,
         oreb_rt = offensive_rebounds / (field_goals_attempted - field_goals_made))

## Compare pbp scores to team box scores 
game_ids <- pbp %>% 
  filter(!is.na(team_id), team_id %in% teams$team_id) %>% 
  mutate(point_value = case_when(
    point_value == 0 & scoring_play == TRUE ~ 2,
    TRUE ~ point_value)) %>% 
  with_groups(.groups = c(game_id, team_id), summarise, score = sum(point_value)) %>% 
  left_join(team_box %>% 
              select(c(game_id, team_id, team_location, team_name, team_score)), by = c("game_id", "team_id")) %>% 
  # Add difference between box score and calculated score
  mutate(diff = team_score - score) %>% 
  # Make sure difference is within 3 points
  filter(abs(diff) <= 3) %>%
  # Add number of teams per game id
  with_groups(game_id, mutate, tms = n()) %>% 
  # Filter for only games with both teams
  filter(tms == 2) %>%  
  select(game_id) %>% 
  distinct()

# Get minutes per game to adjust pace/possessions -------------
minutes <- schedule %>%
  filter(game_id %in% pbp$game_id) %>% 
  select(game_id, status_period) %>% 
  distinct() %>% 
  mutate(mins = 40 + (5 * (status_period - 2))) %>% 
  select(game_id, mins)

# Get points per possession by team for games with adequate information -----------
## Check for duplicates ------
pbp %>% 
  # Filter games in list
  filter(game_id %in% game_ids$game_id) %>% 
  select(game_id, poss_tm, poss_id, poss_xpts) %>% 
  distinct() %>% 
  with_groups(.groups = c(game_id, poss_id), summarise, n = n()) %>%
  filter(n > 1)

## Create summary from play-by-play data  ------
summary <- pbp %>% 
  # Filter games in list
  filter(game_id %in% game_ids$game_id) %>% 
  select(game_id, poss_tm, poss_id, poss_xpts) %>% 
  distinct() %>% 
  # Summarize possessions and expected points 
  with_groups(.groups = c(game_id, poss_tm), summarise, 
              possessions = n(),
              xpoints = sum(poss_xpts)) %>%
  # Add scores from box
  left_join(team_box %>% 
              select(game_id, team_id, game_date, team_score, opponent_team_id,
                     team_home_away, oreb_rt, ast_rt, efg_adj, turnovers),
            by = c("game_id", "poss_tm" = "team_id")) %>% 
  relocate(game_date, .after = game_id) %>% 
  # Add minutes per game
  left_join(minutes, by = "game_id") %>% 
  # Calculate possessions, expected/actual points per possession, expected/actual ratings
  mutate(poss_per_40 = 40 * (possessions / mins),
         xpts_per_poss = xpoints / possessions,
         x_rtg = 100 * xpts_per_poss,
         pts_per_poss = team_score / possessions,
         rtg = 100 * pts_per_poss,
         to_rt = turnovers / possessions) %>% 
  relocate(poss_per_40, .after = possessions) %>% 
  rename(team_id = poss_tm) %>%
  # Get rid of NA game dates
  filter(!is.na(game_date)) %>%
  # Add neutral site/tournament details
  left_join(schedule %>% 
              select(id, neutral_site, venue_id, tournament_id), by = c("game_id" = "id")) %>% 
  arrange(game_date) %>% 
  # Add days rest for each team for each game
  left_join(sched_doubled %>% 
              select(game_id, team_id, days_rest, conference_competition), by = c("game_id", "team_id")) %>% 
  mutate(is_home = case_when(
    neutral_site == TRUE ~ FALSE,
    team_home_away == "home" ~ TRUE,
    team_home_away == "away" ~ FALSE)) %>% 
  select(-c(team_home_away, turnovers))

## Create summary for missing games by using team box
summary_box <- team_box %>%
  filter(game_id %!in% summary$game_id & team_id %in% teams$team_id &
           opponent_team_id %in% teams$team_id) %>% 
  select(game_id, team_id, team_display_name, team_score, team_home_away,
         opponent_team_id, opponent_team_display_name, opponent_team_score,
         assists:turnovers) %>%
  mutate(possessions = field_goals_attempted - offensive_rebounds + turnovers +
           0.475 * (free_throws_attempted),
         rtg = 100 * team_score / possessions,
         pts_per_poss = team_score / possessions,
         efg_adj = (0.5 * free_throws_made + field_goals_made + 0.5 * three_point_field_goals_made) / 
           (field_goals_attempted + free_throws_attempted),
         oreb_rt = offensive_rebounds / (field_goals_attempted - field_goals_made),
         ast_rt = assists / field_goals_made,
         to_rt = turnovers / possessions) %>% 
  left_join(sched_doubled, by = c("game_id", "team_id")) %>% 
  left_join(schedule %>% 
              mutate(mins = 40 + (5 * (status_period - 2))) %>% 
              select(game_id, game_date, game_date_time,
                     tournament_id, mins), by = c("game_id", "game_date")) %>% 
  mutate(game_time = lubridate::hour(game_date_time),
         is_home = case_when(
           neutral_site == TRUE ~ FALSE,
           team_home_away == "home" ~ TRUE,
           team_home_away == "away" ~ FALSE),
         poss_per_40 = possessions / mins * 40) %>%
  relocate(c(game_date, game_date_time, game_time), .after = game_id) %>% 
  arrange(game_date) %>% 
  select(game_id, game_date, team_id, possessions,
         poss_per_40, team_score, opponent_team_id,
         efg_adj, oreb_rt, ast_rt, to_rt, conference_competition,
         mins, pts_per_poss, rtg, neutral_site,
         venue_id, tournament_id, days_rest, is_home)

summary <- bind_rows(summary, summary_box)

## Create shot type summary ------------
shot_type_summary <- pbp %>% 
  # Filter games in list
  filter(game_id %in% game_ids$game_id) %>% 
  # Add shot type (at rim, three, non-rim two) 
  mutate(shot_type = case_when(
    is_atrim == TRUE ~ "rim",
    shot_value == 3 ~ "three",
    shot_value == 1 ~ "ft",
    shooting_play == TRUE & shot_value == 2 & is_atrim == FALSE ~ "nonrim two")) %>% 
  select(game_id, poss_tm, poss_id, shot_type, poss_xpts, point_value, xpts) %>% 
  filter(!is.na(shot_type)) %>% 
  distinct() %>% 
  # Summarize possessions and expected points 
  with_groups(.groups = c(game_id, poss_tm), summarise, 
              fts = sum(shot_type == "ft", na.rm = TRUE),
              pts_ft = sum(point_value[shot_type == "ft"], na.rm = TRUE),
              xpts_ft = sum(xpts[shot_type == "ft"], na.rm = TRUE),
              threes = sum(shot_type == "three", na.rm = TRUE),
              pts_three = sum(point_value[shot_type == "three"], na.rm = TRUE),
              xpts_three = sum(xpts[shot_type == "three"], na.rm = TRUE),
              rim = sum(shot_type == "rim", na.rm = TRUE),
              pts_rim = sum(point_value[shot_type == "rim"], na.rm = TRUE),
              xpts_rim = sum(xpts[shot_type == "rim"], na.rm = TRUE),
              nonrim_2 = sum(shot_type == "nonrim two", na.rm = TRUE),
              pts_nonrim = sum(point_value[shot_type == "nonrim two"], na.rm = TRUE),
              xpts_nonrim = sum(xpts[shot_type == "nonrim two"], na.rm = TRUE)) %>% 
  rename(team_id = poss_tm)

## Add shot type summary to summary -----------
summary <- summary %>% 
  left_join(shot_type_summary, by = c("game_id", "team_id"))

head(summary)

## Check for NA rest values -----
summary %>% 
  filter(is.na(days_rest))

summary %>% 
  filter(is.na(is_home))

summary %>% 
  filter(is.na(conference_competition))

# Load team venues for calculating travel -------------------
all_venues <- readRDS("Stats/Teams/team_venues.rds") %>%
  bind_rows(readRDS("Stats/Teams/neutral_sites.rds")) %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

## Filter venues with no team ID -----------------------------------
venues <- all_venues %>% 
  select(venue_id, latitude, longitude, tz_offset)

## Get team locations -------------------------------------------
team_coords <- all_venues %>% 
  select(team_id, latitude, longitude, tz_offset) %>% 
  distinct() %>% 
  with_groups(.groups = c(team_id), mutate, row = row_number()) %>% 
  filter(row == 1) %>% 
  select(-row)

## Create coordinates data frame ----------------------------
coords <- summary %>% 
  # Add venue coordinates to summary
  left_join(venues %>% 
              select(venue_id, ven_lat = latitude, ven_lng = longitude, ven_tz = tz_offset), by = c("venue_id")) %>%
  # Add team coordinates to summary
  left_join(team_coords %>% 
              mutate(team_id = as.integer(team_id)) %>% 
              select(team_id, 
                     tm_lat = latitude, tm_lng = longitude, tm_tz = tz_offset),
            by = c("team_id"),
            relationship = "many-to-many") %>% 
  # For neutral sites where 
  mutate(ven_lat = case_when(
    neutral_site == TRUE & is.na(ven_lat) ~ tm_lat,
    TRUE ~ ven_lat),
    ven_lng = case_when(
      neutral_site == TRUE & is.na(ven_lng) ~ tm_lng,
      TRUE ~ ven_lng)) %>% 
  distinct()

### Check for NA values --------------------
coords %>% 
  filter(is.na(ven_lat) | is.na(ven_lng) | is.na(tm_lat) | is.na(tm_lng)) #%>%
# If missing values, check for missing venues
#select(venue_id, ven_lat, ven_lng, tm_lat, tm_lng) %>% 
#distinct()

#coords <- coords %>% filter(!is.na(tm_lat))

### Check for duplicates ---------------------
duplicates <- coords %>% 
  with_groups(.groups = c(game_id, team_id), mutate, n = n()) %>% 
  filter(n > 1)

print(length(duplicates$game_id))

## Calculate distances based on coordinates -------------------------------
distances <- data.frame()

for(i in 1:length(coords$tm_lat)){
  
  temp.dist <- ifelse(coords$neutral_site[i] == TRUE,
                      0,
                      (0.621371* sp::spDistsN1(pts = matrix(c(coords$tm_lng[i], coords$tm_lat[i]), ncol = 2),
                                               pt = c(coords$ven_lng[i], coords$ven_lat[i]), longlat = TRUE)))
  
  temp.df <- data.frame(game_id = coords$game_id[i],
                        team_id = coords$team_id[i],
                        travel = temp.dist)
  
  distances <- rbind(distances, temp.df)
  
  if(i %% 1000 == 0 | i == length(coords$tm_lat)){
    print(i)
  }
}

## Add distances to coordinate summary -----------------------
coords <- coords %>% 
  left_join(distances, by = c("game_id", "team_id"))

## Mutate to data frame --------------------------
coords <- as.data.frame(coords)

head(coords, 10)

# Save --------------------------
saveRDS(coords, glue::glue("Stats/Power Ratings/Raw Data/poss_stats_with_types_{cur_yr}.rds"))
