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
priors.test <- priors.all$rtg

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

print(max_games_sched)

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
stat_name <- "rtg"

# Load prior results
res <- readRDS(glue::glue(
  "_Testing/Power Rating Weights/Results/with_priors_{stat_name}_res.rds"))

res.early <- readRDS(glue::glue(
  "_Testing/Power Rating Weights/Results/with_priors_{stat_name}_res_early.rds"))


## Features ----------
list.pace <- c('game_date', 'team_id', 'opp_id')
list.ast <- c('game_date', 'team_id', 'opp_id')
list.to <- c('game_date', 'team_id', 'opp_id')
list.oreb <- c('game_date', 'team_id', 'opp_id')
list.rtg <- c('game_date', 'team_id', 'opp_id', 
              'poss_per_40', 'to_rt', 'oreb_rt', 'ast_rt',
              'is_home', 'neutral_site', 'days_rest', 'travel')

# Set weight parameters for priors ------------------------
prior_wgt <- 0.95
prior_min <- 0.15
max_games <- max_games_sched - 3

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
  
  # Load weights -----
  wgt_pace <- 0.87
  wgt_ast <- 0.98
  wgt_to <- 0.96
  wgt_oreb <- 0.96
  wgt_rtg <- 0.97
  
  # Create ratings ---------------------
  ## Pace ratings
  pace.coeff <- create_ratings(df = summary.train,
                               stat_name = "poss_per_40",
                               name_list = {list.pace},
                               lambda_adj = 0.1,
                               seed = 421,
                               wgt = {wgt_pace})
  
  ## Assist ratings
  ast.coeff <- create_ratings(df = summary.train,
                              stat_name = "ast_rt",
                              name_list = {list.ast},
                              lambda_adj = 0,
                              seed = 421,
                              wgt = {wgt_ast})
  
  
  ## Turnover ratings
  to.coeff <- create_ratings(df = summary.train,
                             stat_name = "to_rt",
                             name_list = {list.to},
                             lambda_adj = 0,
                             seed = 421,
                             wgt = {wgt_to})
  
  
  ## Offensive rebound ratings
  oreb.coeff <- create_ratings(df = summary.train,
                               stat_name = "oreb_rt",
                               name_list = {list.oreb},
                               lambda_adj = 0.05,
                               seed = 421,
                               wgt = {wgt_oreb})
  
  
  
  
  # Create model data frame
  rtg.coeff <- create_ratings(df = summary.train,
                              stat_name = {stat_name},
                              name_list = {list.rtg},
                              seed = 421,
                              lambda_adj = 0,
                              wgt = 0.875)
  
  # Add priors to create ratings ---------
  rtg.coeff <- priors.test %>% 
    full_join(rtg.coeff,
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
  
  ## Get base ratings for each ratings system -------------
  base_pace <- pace.coeff %>% 
    filter(!str_detect(name, "team_") & !str_detect(name, "opp_"))
  
  base_rtg <- rtg.coeff %>% 
    filter(!str_detect(name, "team_") & !str_detect(name, "opp_"))
  
  base_oreb <- oreb.coeff %>% 
    filter(!str_detect(name, "team_") & !str_detect(name, "opp_"))
  
  base_to <- to.coeff %>% 
    filter(!str_detect(name, "team_") & !str_detect(name, "opp_"))
  
  base_ast <- ast.coeff %>% 
    filter(!str_detect(name, "team_") & !str_detect(name, "opp_"))
  
  # Create team ratings -----------------------------------------------------
  ## Pace -----------------
  tm_ratings_pace <- pace.coeff %>% 
    filter(str_detect(name, "team_")) %>% 
    mutate(name = str_remove_all(name, "team_id_"),
           name = as.integer(name),
           value = value + base_pace$value[base_pace$name == "(Intercept)"]) %>% 
    rename(team_id = name,
           OffRtg = value) %>% 
    left_join(pace.coeff %>% 
                filter(str_detect(name, "opp_")) %>% 
                mutate(name = str_remove_all(name, "opp_id_"),
                       name = as.integer(name)) %>% 
                rename(team_id = name,
                       DefRtg = value), by = "team_id") %>% 
    filter(team_id %in% teams$team_id)
  
  ## Offensive Rebounds -----------------
  tm_ratings_oreb <- oreb.coeff %>% 
    filter(str_detect(name, "team_")) %>% 
    mutate(name = str_remove_all(name, "team_id_"),
           name = as.integer(name),
           value = value + base_oreb$value[base_oreb$name == "(Intercept)"]) %>% 
    rename(team_id = name,
           OffRtg = value) %>% 
    left_join(oreb.coeff %>% 
                filter(str_detect(name, "opp_")) %>% 
                mutate(name = str_remove_all(name, "opp_id_"),
                       name = as.integer(name)) %>% 
                rename(team_id = name,
                       DefRtg = value), by = "team_id") %>% 
    filter(team_id %in% teams$team_id)
  
  ## Turnovers -----------------
  tm_ratings_to <- to.coeff %>% 
    filter(str_detect(name, "team_")) %>% 
    mutate(name = str_remove_all(name, "team_id_"),
           name = as.integer(name),
           value = value + base_to$value[base_to$name == "(Intercept)"]) %>% 
    rename(team_id = name,
           OffRtg = value) %>% 
    left_join(to.coeff %>% 
                filter(str_detect(name, "opp_")) %>% 
                mutate(name = str_remove_all(name, "opp_id_"),
                       name = as.integer(name)) %>% 
                rename(team_id = name,
                       DefRtg = value), by = "team_id") %>% 
    filter(team_id %in% teams$team_id)
  
  ## Assists -----------------
  tm_ratings_ast <- ast.coeff %>% 
    filter(str_detect(name, "team_")) %>% 
    mutate(name = str_remove_all(name, "team_id_"),
           name = as.integer(name),
           value = value + base_ast$value[base_ast$name == "(Intercept)"]) %>% 
    rename(team_id = name,
           OffRtg = value) %>% 
    left_join(ast.coeff %>% 
                filter(str_detect(name, "opp_")) %>% 
                mutate(name = str_remove_all(name, "opp_id_"),
                       name = as.integer(name)) %>% 
                rename(team_id = name,
                       DefRtg = value), by = "team_id") %>% 
    filter(team_id %in% teams$team_id)
  
   ## Expected Rating -----------------------------
  tm_ratings_rtg <- rtg.coeff %>% 
    filter(str_detect(name, "team_")) %>% 
    mutate(name = str_remove_all(name, "team_id_"),
           name = as.integer(name),
           value = value + base_rtg$value[base_rtg$name == "(Intercept)"]) %>% 
    rename(team_id = name,
           OffRtg = value) %>% 
    left_join(rtg.coeff %>% 
                filter(str_detect(name, "opp_")) %>% 
                mutate(name = str_remove_all(name, "opp_id_"),
                       name = as.integer(name)) %>% 
                rename(team_id = name,
                       DefRtg = value), by = "team_id") %>% 
    filter(team_id %in% teams$team_id)
  
  ## Rating --------------------------------
  fctrs_rtg <- base_rtg %>% 
    filter(name %in% list.rtg) %>% 
    pivot_wider() %>% 
    rename(pace = poss_per_40,
           to = to_rt, oreb = oreb_rt, ast = ast_rt,
           hfa = is_home, neutral = neutral_site, 
           rest = days_rest, miles = travel)
  
  
  # Create test data
  test.data <- summary %>% 
    filter(game_date == test.date) %>% 
    select(rtg_act = {stat_name}, game_date, game_id, team_id, opp_id, is_home,
           neutral_site, travel, days_rest) %>% 
    left_join(tm_ratings_pace %>% 
                select(team_id, tm_pace = OffRtg), by = "team_id") %>% 
    left_join(tm_ratings_pace %>% 
                select(team_id, opp_pace = DefRtg), by = c("opp_id" = "team_id")) %>% 
    mutate(pace = tm_pace + opp_pace) %>% 
    with_groups(.groups = game_id, mutate, poss_per_40 = mean(pace)) %>% 
    select(-c(pace, tm_pace, opp_pace)) %>% 
    left_join(tm_ratings_oreb %>% 
                select(team_id, tm_oreb = OffRtg), by = "team_id") %>% 
    left_join(tm_ratings_oreb %>% 
                select(team_id, opp_oreb = DefRtg), by = c("opp_id" = "team_id")) %>% 
    mutate(oreb_rt = tm_oreb + opp_oreb) %>% 
    select(-c(tm_oreb, opp_oreb)) %>% 
    left_join(tm_ratings_ast %>% 
                select(team_id, tm_ast = OffRtg), by = "team_id") %>% 
    left_join(tm_ratings_ast %>% 
                select(team_id, opp_ast = DefRtg), by = c("opp_id" = "team_id")) %>% 
    mutate(ast_rt = tm_ast + opp_ast) %>% 
    select(-c(tm_ast, opp_ast)) %>% 
    left_join(tm_ratings_to %>% 
                select(team_id, tm_to = OffRtg), by = "team_id") %>% 
    left_join(tm_ratings_to %>% 
                select(team_id, opp_to = DefRtg), by = c("opp_id" = "team_id")) %>% 
    mutate(to_rt = tm_to + opp_to) %>% 
    select(-c(tm_to, opp_to)) %>% 
    bind_cols(fctrs_rtg) %>% 
    left_join(tm_ratings_rtg %>% 
                select(team_id, OffRtg), by = "team_id") %>% 
    left_join(tm_ratings_rtg %>% 
                select(team_id, DefRtg), by = c("opp_id" = "team_id")) %>% 
    mutate(rtg_est = OffRtg + ({hfa} * is_home) + ({miles} * travel) + ({neutral} * neutral_site) +
             ({to} * to_rt) + ({oreb} * oreb_rt) + ({ast} * ast_rt) +
             ({rest} * days_rest) + ({pace} * poss_per_40) + DefRtg) %>% 
    select(-c(pace:DefRtg))
  
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
#print(res.early)

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
