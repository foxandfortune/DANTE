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
priors.test <- priors.all$to

priors.test <- priors.test %>%
  with_groups(.groups = name,
              mutate,
              row = row_number(),
              max_row = max(row)) %>% 
  filter(row == max_row) %>% 
  select(-contains("row"))

priors.test %>% filter(name == "team_id_91")  

# Create system for building prior weight data frame ----------
schedule <- hoopR::load_mbb_schedule(seasons = {season})

## Filter out conference tournaments/postseason
reg_season <- schedule %>% 
  filter(is.na(tournament_id) |
           !is.na(tournament_id) & conference_competition == FALSE) %>%
  filter(tournament_id %!in% c(35, 11, 22, 21)) %>% 
  select(game_id) %>% 
  distinct()

## Get games to be played for each team -----
games_to_play <- summary %>% 
  filter(game_id %in% reg_season$game_id) %>% 
  mutate(name = paste0("team_id_", team_id)) %>% 
  with_groups(.groups = name,
              summarise,
              games_to_play = n_distinct(game_id)) %>% 
  bind_rows(summary %>% 
              filter(game_id %in% reg_season$game_id) %>% 
              mutate(name = paste0("opp_id_", opp_id)) %>% 
              with_groups(.groups = name,
                          summarise,
                          games_to_play = n_distinct(game_id)))


# Create weights for priors
max_games_sched <- games_to_play %>% 
  filter(!str_detect(name, '-1'), !str_detect(name, '-2')) %>% 
  filter(games_to_play == max(games_to_play)) %>% 
  select(-name) %>% 
  distinct() %>% 
  pull(games_to_play)


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
stat_name <- "to_rt"

# Load prior results
res <- readRDS(glue::glue(
  "_Testing/Power Rating Weights/Results/with_priors_{stat_name}_res.rds"))

res.early <- readRDS(glue::glue(
  "_Testing/Power Rating Weights/Results/with_priors_{stat_name}_res_early.rds"))


## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

# Set weight parameters for priors ------------------------
prior_wgt <- 0.83
prior_min <- 0.05
max_games <- max_games_sched - 2

weight.df <- data.frame(game_no = seq.int(from = 1,
                                          to = max_games,
                                          by = 1)) %>%
  mutate(weight = {prior_wgt} ^ game_no,
         weight = weight / sum(weight),
         cum_weight = cumsum(weight) * (1 - prior_min)) %>% 
  mutate(row = row_number()) %>% 
  select(row, cum_weight)

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
  
  # Get games played by team 
  games_played <- summary.train %>% 
    mutate(name = paste0("team_id_", team_id)) %>% 
    with_groups(.groups = name,
                summarise,
                games_played = n_distinct(game_id)) %>% 
    bind_rows(summary.train %>% 
                mutate(name = paste0("opp_id_", opp_id)) %>% 
                with_groups(.groups = name,
                            summarise,
                            games_played = n_distinct(game_id)))
  
  # Create model data frame
  raw.coeff <- create_ratings(df = summary.train,
                              stat_name = {stat_name},
                              name_list = {list},
                              seed = 421,
                              lambda_adj = 0,
                              wgt = 0.875)
  
  # Add priors to create ratings ---------
  raw.coeff <- priors.test %>% 
    full_join(raw.coeff,
              by = "name",
              suffix = c("_prior", "_curr")) %>% 
    left_join(games_played, by = "name") %>%
    mutate(games_played = case_when(
      is.na(games_played) ~ round(mean(games_played, na.rm = TRUE), 2),
      TRUE ~ games_played)) %>% 
    left_join(games_to_play, by = "name") %>% 
    left_join(weight.df, by = c("games_played" = "row")) %>% 
    mutate(cum_weight = replace_na(cum_weight,
                                   max(cum_weight, na.rm = TRUE))) %>% 
    mutate(value = case_when(
      is.na(value_curr) ~ value_prior,
      is.na(value_prior) ~ value_curr,
      str_detect(name, "team_id_") | str_detect(name, "opp_id_") ~ 
        cum_weight * value_curr + (1 - cum_weight) * value_prior,
      TRUE ~ (0.9 + 0.1 * games_played / max_games) * value_curr + 
        (0.1 - 0.1 * games_played / max_games) * value_prior)) %>% 
    select(name, value)
  
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

test.early <- test.summary %>% 
  filter(game_date %in% dates.early)

## Add results to data frame --------
#res <- data.frame(
#  stat = {stat_name}, season = {season}, 
#  prior_wgt = {prior_wgt},
#  prior_min = {prior_min},
#  max_games = {max_games},
#  corr = round(c(cor(test.summary$rtg_act, test.summary$rtg_est)), 4),
#  rmse = round(Metrics::rmse(test.summary$rtg_act, test.summary$rtg_est), 4))

#res.early <- data.frame(data.frame(
#  stat = {stat_name}, season = {season}, 
#  prior_wgt = {prior_wgt},
#  prior_min = {prior_min},
#  max_games = {max_games},
#  corr = round(c(cor(test.early$rtg_act, test.early$rtg_est)), 4),
#  rmse = round(Metrics::rmse(test.early$rtg_act, test.early$rtg_est), 4)))

#print(res)

res <- res %>% 
  bind_rows(data.frame(
    stat = {stat_name}, season = {season}, 
    prior_wgt = {prior_wgt},
    prior_min = {prior_min},
    max_games = {max_games},
    corr = round(c(cor(test.summary$rtg_act, test.summary$rtg_est)), 4),
    rmse = round(Metrics::rmse(test.summary$rtg_act, test.summary$rtg_est), 4)))

res.early <- res.early %>% 
  bind_rows(data.frame(data.frame(
    stat = {stat_name}, season = {season}, 
    prior_wgt = {prior_wgt},
    prior_min = {prior_min},
    max_games = {max_games},
    corr = round(c(cor(test.early$rtg_act, test.early$rtg_est)), 4),
    rmse = round(Metrics::rmse(test.early$rtg_act, test.early$rtg_est), 4))))

print(res)
print(res.early)

saveRDS(res,
        glue::glue("_Testing/Power Rating Weights/Results/with_priors_{stat_name}_res.rds"))

saveRDS(res.early,
        glue::glue("_Testing/Power Rating Weights/Results/with_priors_{stat_name}_res_early.rds"))



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
