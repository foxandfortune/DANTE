library(hoopR)
library(tidyverse)
library(glmnet)
library(sp)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
setwd('..')

# Set season/country/tier
season <- 2024

# Negate in formula
`%!in%` = Negate(`%in%`)

# Load teams and summary data -----
teams <- readRDS(glue::glue("Teams/team_database.rds"))

summary <- readRDS(glue::glue("Power Ratings/Raw Data/poss_stats_with_types_{season}.rds")) %>% 
  # Filter out teams in team dataset
  #filter(team_id %in% teams$team_id) %>% 
  with_groups(game_id, mutate, tm_ct = n()) %>%
  # Only include games where both teams are in teams dataset
  filter(tm_ct == 2) %>% 
  select(-tm_ct) %>% 
  as.data.frame() %>%
  rename(opp_id = opponent_team_id)

### Check for NA values
summary %>% 
  filter(is.na(days_rest))

# Load priors ---------------
priors.all <- readRDS(glue::glue('Power Ratings/Team Ratings/Priors/priors_{season}.rds'))

## Set priors for testing ------------
priors.test <- priors.all$pace

priors.test <- priors.test %>%
  with_groups(.groups = name,
              mutate,
              row = row_number(),
              max_row = max(row)) %>% 
  filter(row == max_row) %>% 
  select(-contains("row"))

priors.test %>% filter(name == "team_id_91")  

priors.test <- priors.test %>%
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_"))) %>% 
  rename(OffRtg = value) %>% 
  left_join(priors.test %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_"))) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  select(team_id, OffRtg, DefRtg) %>% 
  mutate(OffRtg = round(OffRtg, 4),
         DefRtg = round(DefRtg, 4),
         PowerRtg = OffRtg - DefRtg) %>% 
  arrange(desc(PowerRtg))


# Get dates for testing --------
dates <- summary %>%
  select(game_date) %>%
  arrange(game_date) %>% 
  filter(game_date >= min(game_date) + 14) %>% 
  distinct()

dates.early <- dates %>% 
  slice(1:30) %>% 
  pull(game_date)

# Load create ratings function
create_ratings <- readRDS("Tools/create_ratings_stat.rds")

# Set stat for testing ----------
stat_name <- "poss_per_40"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

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
                              lambda_adj = 0,
                              wgt = 0.875)
  
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
    select(rtg_act = {stat_name}, game_date, team_id, opp_id, is_home,
           poss_per_40,
           neutral_site, travel, days_rest) %>% 
    mutate(start = {base}) %>% 
    left_join(tm_ratings %>% 
                select(team_id, OffRtg), by = "team_id") %>% 
    left_join(tm_ratings %>% 
                select(team_id, DefRtg), by = c("opp_id" = "team_id")) %>% 
    mutate(rtg_est = round(start + OffRtg + DefRtg, 2)) %>% 
    select(-c(start:DefRtg))
  
  test.priors <- summary %>% 
    mutate(team_id = as.character(team_id),
           opp_id = as.character(opp_id)) %>% 
    filter(game_date == test.date) %>% 
    select(rtg_act = {stat_name}, game_date, team_id, opp_id, is_home,
           poss_per_40,
           neutral_site, travel, days_rest) %>% 
    mutate(start = {base}) %>% 
    left_join(priors.test %>% 
                select(team_id, OffRtg), by = "team_id") %>% 
    left_join(priors.test %>% 
                select(team_id, DefRtg), by = c("opp_id" = "team_id")) %>% 
    mutate(rtg_est_priors = round(start + OffRtg + DefRtg, 2)) %>% 
    select(-c(start:DefRtg))
  
  test.data <- test.data %>% 
    left_join(test.priors %>% 
                select(game_date, team_id, rtg_est_priors), 
              by = c("game_date", "team_id"))
  
  test.summary <- rbind(test.summary, test.data)
  
  print(i)
  
  rm(test.date, raw.coeff, base, tm_ratings, test.data)
  
  #Sys.sleep(0.5)
}

# Review results --------------
head(test.summary)

## Filter NAs
test.summary <- test.summary %>% 
  filter(!is.na(rtg_est), !is.na(rtg_est_priors))

## Save file ---------
saveRDS(test.summary,
        glue::glue('_Testing/Power Rating Weights/Results/test.summary_{stat_name}.rds'))


# Look at trends ---------
test.summary %>% 
  with_groups(.groups = team_id,
              mutate,
              game_no = row_number()) %>% 
  with_groups(.groups = game_no,
              summarise,
              prior_rmse = Metrics::rmse(rtg_act, rtg_est_priors),
              current_rmse = Metrics::rmse(rtg_act, rtg_est)) %>% 
  ggplot() +
  geom_line(aes(x = game_no, y = prior_rmse), color = "red") +
  geom_line(aes(x = game_no, y = current_rmse), color = "blue") 
  



test.summary %>% 
  ggplot(aes(x = rtg_est, y = rtg_act)) + geom_point()+ geom_abline() + geom_smooth(method = lm)

test.summary %>% 
  mutate(diff = rtg_act - rtg_est) %>% 
  ggplot(aes(x = diff)) + geom_histogram()

test.summary %>% 
  mutate(diff = rtg_act - rtg_est) %>% 
  summarise(avg = mean(diff),
            mid = median(diff),
            var = sd(diff))

test.summary %>% 
  mutate(diff = rtg_act - rtg_est) %>% 
  with_groups(.groups = c(team_id), summarise,
              games = n(),
              var = sd(diff)) %>%
  left_join(teams %>% 
              select(team, team_id), by = "team_id") %>% 
  arrange(var) %>% 
  filter(team == "Gonzaga")
