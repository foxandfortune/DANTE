library(hoopR)
library(tidyverse)
library(sp)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
setwd('..')

# Add NOT IN function:
`%!in%` = Negate(`%in%`)

# Set reference year
cur_yr <- 2025

# Load teams and venue data -----
teams <- readRDS(glue::glue("Stats/Teams/team_database.rds"))

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

# Load schedule ------ 
schedule <- hoopR::load_mbb_schedule(seasons = {cur_yr}) %>% 
  as.data.frame() %>% 
  # Filter for teams in database
  filter(home_id %in% teams$team_id | away_id %in% teams$team_id) %>% 
  # Exclude canceled/postponed games
  filter(status_type_description %!in% c("Postponed", "Canceled"))

# Calculate days rest based on schedule ---------------
sched_doubled <- schedule %>% 
  select(game_id = id, game_date, team_id = home_id, venue_id, neutral_site) %>% 
  bind_rows(schedule %>% 
              select(game_id = id, game_date, team_id = away_id, venue_id, neutral_site)) %>% 
  distinct() %>% 
  arrange(game_date) %>% 
  with_groups(.groups = c(team_id), mutate, days_rest = as.numeric(game_date - lag(game_date))) %>% 
  # Set NA values to 7 days; most for start of season games
  mutate(days_rest = replace_na(days_rest, 7))

## Create coordinates data frame ----------------------------
coords <- sched_doubled %>% 
  # Add venue coordinates to summary
  left_join(venues %>% 
              select(venue_id, ven_lat = latitude, ven_lng = longitude, ven_tz = tz_offset),
            by = c("venue_id")) %>%
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
  distinct() %>% 
  filter(team_id %in% teams$team_id)

### Check for NA values --------------------
coords %>% 
  filter(is.na(ven_lat) | is.na(ven_lng) | is.na(tm_lat) | is.na(tm_lng)) #%>% 
  # If missing values, check for missing venues
  #select(venue_id) %>% 
  #distinct()

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
  left_join(distances, by = c("game_id", "team_id")) %>% 
  arrange(game_date, game_id)

# Get rid of extra columns for coords --------------------------
schedule_adj <- schedule %>% 
  left_join(coords %>% 
              select(game_date, game_id, team_id,
                     home_rest = days_rest,
                     home_dist = travel), by = c("game_date", "game_id",
                                                "home_id" = "team_id")) %>% 
  left_join(coords %>% 
              select(game_date, game_id, team_id,
                     away_rest = days_rest,
                     away_dist = travel), by = c("game_date", "game_id",
                                                 "away_id" = "team_id")) %>% 
  select(game_date, game_id,
         home_id, home_rest, home_dist,
         away_id, away_rest, away_dist) %>% 
  mutate(home_dist = round(home_dist, digits = 0),
         away_dist = round(away_dist, digits = 0)) %>% 
  filter(home_id %in% teams$team_id & away_id %in% teams$team_id)

head(schedule_adj %>% 
       arrange(game_date))

# Check for NA values
schedule_adj %>% 
  filter(is.na(home_rest) | is.na(away_rest) | 
           is.na(home_dist) | is.na(away_dist))

# Save --------------------------
saveRDS(schedule_adj, glue::glue("Minos/Season Simulation Backup/schedule_adj_{cur_yr}.rds"))
