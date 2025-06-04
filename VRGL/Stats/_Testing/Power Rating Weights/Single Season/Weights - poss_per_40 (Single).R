library(hoopR)
library(tidyverse)
library(glmnet)
library(sp)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
setwd('..')
setwd('..')
setwd('..')
setwd('..')

# Set season/country/tier
season <- 2025

# Negate in formula
`%!in%` = Negate(`%in%`)

# Load teams and summary data -----
teams <- readRDS(glue::glue("_Helper Files/Team Data/team_database.rds"))

summary <- readRDS(glue::glue("VRGL/Stats/Team and Player Stats/Power Ratings/Raw Data/poss_stats_with_types_{season}.rds")) %>% 
  with_groups(game_id, mutate, tm_ct = n()) %>%
  # Only include games where both teams are in teams dataset
  filter(tm_ct == 2) %>% 
  select(-tm_ct) %>% 
  as.data.frame() %>%
  rename(opp_id = opponent_team_id)

### Check for NA values
summary %>% 
  filter(is.na(days_rest))

# Get dates for testing --------
dates <- summary %>%
  select(game_date) %>%
  arrange(game_date) %>% 
  slice(round((.3 * length(summary$game_date) + 1), 0):n()) %>% 
  distinct()

# Load create ratings function
create_ratings <- readRDS("_Helper Files/Other/create_ratings_stat.rds")

# Select stat name to project and team/home field factors --------- 
glimpse(summary)

# Need for this verison: days_rest, travel -----
## Predictive stat -----
# Shares (FT/Rim/NonRim/Three); Conversion (Rim/NonRim/Three)
stat_name <- "poss_per_40"

## Stat to be predicted ------
stat_est <- "poss_per_40"

# Load prior results
res_all <- readRDS(glue::glue(
  "VRGL/Stats/_Testing/Power Rating Weights/_Results/single_season_{stat_name}_res.rds"))

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

# Set weight for testing; will run for weights 0.90 through 1.00 to start -----
wgt_var <- 0.85

# Set lambda_adj
lambda_adj <- 0.05

# Create test.summary to start ----------
test.summary <- data.frame()

## Loop through dates ----------------
for(i in 1:length(dates$game_date)){
  # select date
  test.date <- dates$game_date[i]
  
  # Filter summary to get games before test_date
  summary.train <- summary %>%
    # filter for dates 
    filter(game_date < test.date)
  
  # Create model data frame
  raw.coeff <- create_ratings(df = summary.train,
                              stat_name = {stat_name},
                              name_list = {list},
                              seed = 421,
                              lambda_adj = {lambda_adj},
                              wgt = {wgt_var})
  
  # Get intercept and home field advantage
  base <- raw.coeff$value[raw.coeff$name == "(Intercept)"]
  
  # Use coefficients to create power ratings
  tm_ratings <- raw.coeff %>% 
    filter(str_detect(name, "team_id_")) %>% 
    mutate(team_id = str_remove_all(name, c("team_id_"))) %>% 
    rename(OffRtg = value) %>% 
    left_join(raw.coeff %>% 
                filter(str_detect(name, "opp_id_")) %>% 
                mutate(team_id = str_remove_all(name, c("opp_id_"))) %>% 
                rename(DefRtg = value), by = "team_id") %>% 
    select(team_id, OffRtg, DefRtg) %>% 
    mutate(OffRtg = round(OffRtg, 4),
           DefRtg = round(DefRtg, 4),
           PowerRtg = OffRtg - DefRtg) %>% 
    arrange(desc(PowerRtg))
  
  test.data <- summary %>% 
    mutate(team_id = as.character(team_id),
           opp_id = as.character(opp_id)) %>% 
    filter(game_date == test.date) %>% 
    select(rtg_act = {stat_est}, game_date, team_id, opp_id, is_home,
           poss_per_40,
           neutral_site, travel, days_rest) %>% 
    mutate(start = {base}) %>% 
    left_join(tm_ratings %>% 
                select(team_id, OffRtg), by = "team_id") %>% 
    left_join(tm_ratings %>% 
                select(team_id, DefRtg), by = c("opp_id" = "team_id")) %>% 
    mutate(rtg_est = round(start + OffRtg + DefRtg, 2)) %>% 
    select(-c(start:DefRtg))
  
  test.summary <- rbind(test.summary, test.data)
  
  print(i)
  
  rm(test.date, raw.coeff, base, tm_ratings, test.data)
  
  #Sys.sleep(0.5)
}

# Review results --------------
head(test.summary)

## Filter NAs
test.summary <- test.summary %>% 
  filter(!is.na(rtg_est))

## Add results to data frame --------
res <- data.frame(season = {season}, stat = {stat_name},
                  wgt = {wgt_var}, lambda_adj = {lambda_adj}, 
                  corr = round(c(cor(test.summary$rtg_act, test.summary$rtg_est)), 4),
                  rmse = round(sqrt(mean((test.summary$rtg_act - test.summary$rtg_est)^2)), 4))

print(res)

## Add to prior results
res_all <- bind_rows(res_all,
                     res)

print(res_all %>%
        arrange(rmse))

## Get best by year -----
res_best <- res_all %>% 
  with_groups(.groups = c(season),
              mutate,
              rank = dense_rank(rmse)) %>% 
  arrange(rank, desc(corr)) %>% 
  with_groups(.groups = season,
              slice,
              1) %>% 
  select(-rank)

print(res_best)

# Save files ------------
## Results -------------
saveRDS(res_all,
        glue::glue(
          "VRGL/Stats/_Testing/Power Rating Weights/_Results/single_season_{stat_name}_res.rds"))

## Weights (for creating power ratings) ------------
saveRDS(res_best,
        glue::glue(
          "VRGL/Stats/Team and Player Stats/Power Ratings/Weights/single_season_{stat_name}_wgt.rds"))

