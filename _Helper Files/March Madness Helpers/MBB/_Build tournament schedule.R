library(hoopR)
library(sp)
library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
setwd('..')

# Add NOT IN function:
`%!in%` = Negate(`%in%`)

# Set season -----------------------
season <- 2024

# Load teams -------------------------------------------
teams <- readRDS('Stats/Teams/team_database.rds') %>% 
  mutate(team_id = as.integer(team_id))

# Load schedule -------------------
sched <- hoopR::load_mbb_schedule(seasons = {season})

## Filter tournament games -----------------------
tourn <- sched %>% 
  filter(tournament_id == 22) %>% 
  arrange(date)

# Load team venues for calculating travel -------
venues <- readRDS("Stats/Teams/team_venues.rds") %>%
  plyr::rbind.fill(readRDS("Stats/Teams/neutral_sites.rds")) %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

# Filter teams in First Four and First Round -------------------------------
participants_raw <- tourn %>% 
  filter(str_detect(notes_headline, "First Four") | str_detect(notes_headline, "1st Round")) %>% 
  mutate(region = case_when(
    str_detect(notes_headline, "South Region") ~ "South",
    str_detect(notes_headline, "West Region") ~ "West",
    str_detect(notes_headline, "Midwest Region") ~ "Midwest",
    str_detect(notes_headline, "East Region") ~ "East")) %>%
  select(region,
         team_id = away_id,
         team_name = away_short_display_name) %>% 
  bind_rows(tourn %>% 
              filter(str_detect(notes_headline, "First Four") | str_detect(notes_headline, "1st Round")) %>% 
              mutate(region = case_when(
                str_detect(notes_headline, "South Region") ~ "South",
                str_detect(notes_headline, "West Region") ~ "West",
                str_detect(notes_headline, "Midwest Region") ~ "Midwest",
                str_detect(notes_headline, "East Region") ~ "East")) %>%
              select(region,
                     team_id = home_id,
                     team_name = home_short_display_name)) %>% 
  distinct() %>% 
  arrange(region, team_name) %>% 
  filter(team_name != "TBD")

## Go through tournament history to get team IDs for each seed  (IF SEED NOT IN SCHEDULE NAMES)--------------------
participants_raw %>% 
  print(n = Inf)

tourney_teams <- data.frame(region = c(rep("East", 16),
                      rep("Midwest", 18),
                      rep("South", 17),
                      rep("West", 17)),
           seed = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                    11, 12, 13, 14, 15, 16,
                    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10,
                    11, 12, 13, 14, 15, 16, 16,
                    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10,
                    11, 12, 13, 14, 15, 16,
                    1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                    11, 12, 13, 14, 15, 16, 16),
           team_id =c(130, 333, 251, 52, 38, 252, 41, 99, 179, 120,
                      127, 26, 46, 2430, 2000, 314, 116, 2640,
                      356, 248, 277, 197, 2633, 21, 228, 2350, 59, 164,
                      183, 204, 2335, 2413, 325, 2182,
                      239, 194, 8, 2509, 222, 2641, 57, 153, 275, 259,
                      328, 2737, 249, 2142, 198, 42,
                      2250, 2294, 2305, 258, 156, 30, 2483, 201, 142, 2670,
                      2724, 2181, 2540, 195, 331, 2253, 2450, 2026)) %>% 
  arrange(region, seed) %>% 
  distinct() %>% 
  with_groups(.groups = c(region, seed), mutate, n = n()) %>%
  mutate(first_four = case_when(
    n == 2 ~ TRUE,
    TRUE ~ FALSE)) %>% 
  select(-n) %>% 
  mutate(winner_to = case_when(
    seed %in% c(1, 16) ~ "16 v 1",
    seed %in% c(2, 15) ~ "15 v 2",
    seed %in% c(3, 14) ~ "14 v 3",
    seed %in% c(4, 13) ~ "13 v 4",
    seed %in% c(5, 12) ~ "12 v 5",
    seed %in% c(6, 11) ~ "11 v 6",
    seed %in% c(7, 10) ~ "10 v 7",
    seed %in% c(8, 9) ~ "9 v 8")) %>% 
  left_join(teams %>% 
              select(team_id, team_name = team), by = "team_id") %>% 
  relocate(team_name, .after = team_id)

# First: Select the teams by seed and bracket (IF SEED IN SCHEDULE COLUMNS) ---------
tourney_teams <- tourn %>% 
  filter(str_detect(notes_headline, "First Four") | str_detect(notes_headline, "1st Round")) %>% 
  mutate(region = case_when(
    str_detect(notes_headline, "South Region") ~ "South",
    str_detect(notes_headline, "West Region") ~ "West",
    str_detect(notes_headline, "Midwest Region") ~ "Midwest",
    str_detect(notes_headline, "East Region") ~ "East",
  )) %>% 
  select(region,
         seed = away_current_rank,
         team_id = away_id,
         team_name = away_short_display_name) %>% 
  bind_rows(tourn %>% 
              filter(str_detect(notes_headline, "First Four") | str_detect(notes_headline, "1st Round")) %>% 
              mutate(region = case_when(
                str_detect(notes_headline, "South Region") ~ "South",
                str_detect(notes_headline, "West Region") ~ "West",
                str_detect(notes_headline, "Midwest Region") ~ "Midwest",
                str_detect(notes_headline, "East Region") ~ "East",
              ))%>% 
              select(region,
                     seed = home_current_rank, 
                     team_id = home_id,
                     team_name = home_short_display_name)) %>% 
  arrange(region, seed) %>% 
  distinct() %>% 
  with_groups(.groups = c(region, seed), mutate, n = n()) %>%
  mutate(first_four = case_when(
    n == 2 ~ TRUE,
    TRUE ~ FALSE)) %>% 
  select(-n) %>% 
  mutate(winner_to = case_when(
    seed %in% c(1, 16) ~ "16 v 1",
    seed %in% c(2, 15) ~ "15 v 2",
    seed %in% c(3, 14) ~ "14 v 3",
    seed %in% c(4, 13) ~ "13 v 4",
    seed %in% c(5, 12) ~ "12 v 5",
    seed %in% c(6, 11) ~ "11 v 6",
    seed %in% c(7, 10) ~ "10 v 7",
    seed %in% c(8, 9) ~ "9 v 8")) %>% 
  as.data.frame() %>% 
  filter(team_id != -2)

str(tourney_teams)

tourney_teams

# Create Tournament Structure ----------------
bracket.left <- c("West", "East")
bracket.right <- c("South", "Midwest")

tourney_structure <- data.frame(
  region = c(rep("South", 15),
             rep("East", 15),
             rep("West", 15),
             rep("Midwest", 15),
             "Left", "Right", "All"),
  round_name = c(rep("First", 8),
            rep("Second", 4),
            rep("Sweet 16", 2),
            rep("Elite 8", 1),
            rep("First", 8),
            rep("Second", 4),
            rep("Sweet 16", 2),
            rep("Elite 8", 1),
            rep("First", 8),
            rep("Second", 4),
            rep("Sweet 16", 2),
            rep("Elite 8", 1),
            rep("First", 8),
            rep("Second", 4),
            rep("Sweet 16", 2),
            rep("Elite 8", 1),
            rep("Final 4", 2),
            "Championship"),
  game = c(rep(c("16 v 1", "9 v 8", "12 v 5", "13 v 4",
                 "11 v 6", "14 v 3", "10 v 7", "15 v 2",
                 "32-1", "32-2", "32-3", "32-4",
                 "16-1", "16-2", "8-1"), 4),
           "4-1", "4-2", "Final")
) %>% 
  mutate(round_no = case_when(
    round_name == "First" ~ 1,
    round_name == "Second" ~ 2,
    round_name == "Sweet 16" ~ 3,
    round_name == "Elite 8" ~ 4,
    round_name == "Final 4" ~ 5,
    round_name == "Championship" ~ 6),
    winner_to = case_when(
      game %in% c("16 v 1", "9 v 8") ~ "32-1",
      game %in% c("12 v 5", "13 v 4") ~ "32-2",
      game %in% c("11 v 6", "14 v 3") ~ "32-3",
      game %in% c("10 v 7", "15 v 2") ~ "32-4",
      game %in% c("32-1", "32-2") ~ "16-1",
      game %in% c("32-3", "32-4") ~ "16-2",
      game %in% c("16-1", "16-2") ~ "8-1",
      game == "8-1" & region %in% bracket.left ~ "4-1",
      game == "8-1" & region %in% bracket.right ~ "4-2",
      game %in% c("4-1", "4-2") ~ "Final",
      TRUE ~ NA)) %>% 
  relocate(round_no, .after = round_name)

tourney_structure


# Get First Four Schedule ---------------------------
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


first_four_venue <- tourn %>% 
  filter(str_detect(notes_headline, "First Four")) %>% 
  mutate(region = case_when(
    str_detect(notes_headline, "South Region") ~ "South",
    str_detect(notes_headline, "West Region") ~ "West",
    str_detect(notes_headline, "Midwest Region") ~ "Midwest",
    str_detect(notes_headline, "East Region") ~ "East",
  )) %>% 
  select(region, venue_id, away_id) %>%
  left_join(tourney_teams %>% 
              filter(first_four == TRUE) %>% 
              select(region, seed, team_id), by = c("region", "away_id" = "team_id")) %>% 
  # Add venue coordinates to summary
  left_join(venues %>% 
              select(venue_id, ven_lat = latitude, ven_lng = longitude), by = c("venue_id")) %>% 
  distinct() %>% 
  select(-c(away_id))

## Find seeds of First Four by Region ------
first_four_seeds <- tourney_teams %>% 
  filter(first_four == TRUE) %>% 
  select(region, seed) %>% 
  distinct()

# Create First Four schedule -----------------
first_four_sched <- tourn %>% 
  filter(str_detect(notes_headline, "First Four")) %>%
  mutate(region = case_when(
    str_detect(notes_headline, "South Region") ~ "South",
    str_detect(notes_headline, "West Region") ~ "West",
    str_detect(notes_headline, "Midwest Region") ~ "Midwest",
    str_detect(notes_headline, "East Region") ~ "East",
  )) %>% 
  left_join(first_four_seeds, by = "region", relationship = "many-to-many") %>% 
  select(region, venue_id,
         away_seed = seed,
         away_id, away_score,
         home_seed = seed,
         home_id, home_score) %>%
  left_join(tourney_teams %>% 
              filter(first_four == TRUE) %>% 
              select(region, seed, team_id, team_name), by = c("region", "away_seed" = "seed", "away_id" = "team_id")) %>% 
  filter(!is.na(team_name)) %>% 
  select(-team_name) %>% 
  # Add team coordinates to summary
  left_join(team_coords %>% 
              mutate(team_id = as.integer(team_id)) %>% 
              select(team_id, home_lat = latitude, home_lng = longitude),
            by = c("home_id" = "team_id")) %>% 
  left_join(team_coords %>% 
              mutate(team_id = as.integer(team_id)) %>% 
              select(team_id, away_lat = latitude, away_lng = longitude),
            by = c("away_id" = "team_id")) %>% 
  left_join(first_four_venue, by = c("region", "venue_id", "away_seed" = "seed")) %>% 
  mutate(away_rest = 2,
         home_rest = 2) %>% 
  mutate(round_name = "First Four",
         round_no = 0,
         result = home_score - away_score,
         winner_to = paste0(away_seed, " v ", (17-away_seed))) %>% 
  relocate(c(round_name, round_no), .after = region) %>% 
  as.data.frame() %>% 
  distinct()

first_four_sched

## Calculate distances ------
distance <- data.frame()

for(i in 1:4){
  home.temp <- round((0.621371 * sp::spDistsN1(pts = matrix(c(first_four_sched$home_lng[i],
                                                              first_four_sched$home_lat[i]), ncol = 2),
                                              pt = c(first_four_sched$ven_lng[i],
                                                     first_four_sched$ven_lat[i]),
                                              longlat = TRUE)),0)
  
  away.temp = round((0.621371 * sp::spDistsN1(pts = matrix(c(first_four_sched$away_lng[i],
                                                             first_four_sched$away_lat[i]), ncol = 2),
                                              pt = c(first_four_sched$ven_lng[i],
                                                     first_four_sched$ven_lat[i]),
                                              longlat = TRUE)),0)
  
  temp <- data.frame(home_dist = home.temp,
                     away_dist = away.temp)
  
  distance = bind_rows(distance,
                       temp)
}

first_four_sched <- first_four_sched %>% 
  bind_cols(distance) %>% 
  relocate(c(away_id, away_rest, away_dist), .after = away_seed) %>% 
  relocate(c(home_id, home_rest, home_dist), .after = home_seed) %>% 
  select(-contains("_lng"), -contains("_lat"), -venue_id)

first_four_sched

# Get venues First Four winners will travel to --------
##### UPDATE BASED ON FIRST FOUR SETTINGS #############
first_four_travel <- tourn %>% 
  filter(str_detect(notes_headline, "1st Round")) %>%
  mutate(region = case_when(
    str_detect(notes_headline, "South Region") ~ "South",
    str_detect(notes_headline, "West Region") ~ "West",
    str_detect(notes_headline, "Midwest Region") ~ "Midwest",
    str_detect(notes_headline, "East Region") ~ "East",
  )) %>% 
  left_join(tourney_teams %>% 
              select(region, team_id, seed, first_four), by = c("region", "away_id" = "team_id")) %>% 
  filter(is.na(first_four)) %>% 
  select(region, seed, venue_id) %>% 
  left_join(venues %>%
              select(venue_id, ven_lat = latitude, ven_lng = longitude), by = c("venue_id")) %>% 
  mutate(seed = case_when(
    region == "West" ~ 16,
    region == "South" ~ 10,
    venue_id == 1893 ~ 10,
    venue_id == 2183 ~16)) %>% 
  mutate(game_to = paste0(seed, " v ", 17 - seed)) %>% 
  left_join(first_four_venue, by = c("region", "seed"), suffix = c("", "_start"))



## Calculate distances ------
distance.first <- data.frame()

for(i in 1:4){
  travel.temp <- round((0.621371 * sp::spDistsN1(pts = matrix(c(first_four_travel$ven_lng[i],
                                                                first_four_travel$ven_lat[i]), ncol = 2),
                                               pt = c(first_four_travel$ven_lng_start[i],
                                                      first_four_travel$ven_lat_start[i]),
                                               longlat = TRUE)),0)
  
  temp <- data.frame(away_dist = travel.temp)
  
  distance.first = bind_rows(distance.first,
                             temp)
}


first_four_travel <- first_four_travel %>% 
  bind_cols(distance.first) %>% 
  select(region, away_dist, game_to)


## Add travel for First Four teams to tourney structure -------
tourney_structure <- tourney_structure %>% 
  left_join(first_four_travel, by = c("region", "game" = "game_to")) %>% 
  mutate(away_dist = replace_na(away_dist, 0),
         home_dist = 0)


# Save tourney structure, tourney teams, and First Four schedule ----------
saveRDS(tourney_teams, glue::glue("Minos/March Madness Backup/tourney_teams_{season}.rds"))
saveRDS(first_four_sched, glue::glue("Minos/March Madness Backup/first_four_{season}.rds"))
saveRDS(tourney_structure, glue::glue("Minos/March Madness Backup/tourney_structure_{season}.rds"))

