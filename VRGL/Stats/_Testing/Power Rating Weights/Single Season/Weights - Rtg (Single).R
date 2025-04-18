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
  as.data.frame()

### Check for NA values
summary %>% 
  filter(is.na(days_rest))

# Calculate shot share by type; shot conversions by type ---------
summary <- summary %>% 
  rename(opp_id = opponent_team_id)

# Get dates for testing --------
dates <- summary %>%
  arrange(game_date) %>% 
  select(game_date) %>% 
  slice(round((.3 * length(summary$game_date) + 1), 0):n()) %>% 
  distinct()

# Load create ratings function
create_ratings <- readRDS("Tools/create_ratings_stat.rds")

# Select stat name to project and team/home field factors --------- 
glimpse(summary)

# Need for this verison: days_rest, travel -----
## Predictive stat -----
# Shares (FT/Rim/NonRim/Three); Conversion (Rim/NonRim/Three)
stat_name <- "rtg"

## Stat to be predicted ------
stat_est <- "rtg"

# Load prior results
res <- readRDS(glue::glue("_Testing/Power Rating Weights/Results/single_season_{stat_name}_with_proj_res.rds"))

print(res)

## Features ----------
list <- c('game_date', 'team_id', 'opp_id', 
          'poss_per_40', 'to_rt', 'oreb_rt', 'ast_rt',
          'is_home', 'neutral_site', 'days_rest', 'travel')

# Set weight for testing; will run for weights 0.90 through 1.00 to start -----
wgt_var <- 0.92

## Set the lambda adj ----------------
lambda_adj <- 1.75

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
  
  ## Features ----------
  list.pace <- c('game_date', 'team_id', 'opp_id')
  list.ast <- c('game_date', 'team_id', 'opp_id')
  list.to <- c('game_date', 'team_id', 'opp_id')
  list.oreb <- c('game_date', 'team_id', 'opp_id')
  list.rtg <- c('game_date', 'team_id', 'opp_id', 
                'poss_per_40', 'to_rt', 'oreb_rt', 'ast_rt',
                'is_home', 'neutral_site', 'days_rest', 'travel')
  
  # Load weights -----
  wgt_pace <- 0.87
  wgt_ast <- 0.98
  wgt_to <- 0.96
  wgt_oreb <- 0.96
  wgt_rtg <- {wgt_var}
  
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
  
  
  ## Ratings
  rtg.coeff <- create_ratings(df = summary.train,
                              stat_name = "rtg",
                              name_list = {list.rtg},
                              lambda_adj = {lambda_adj},
                              seed = 421,
                              wgt = {wgt_rtg})
  
  
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
    filter(name %in% c("days_rest", "neutral_site", "is_home", "travel", "poss_per_40",
                       "oreb_rt", "to_rt", "ast_rt")) %>% 
    pivot_wider() %>% 
    rename(pace = poss_per_40,
           to = to_rt, oreb = oreb_rt, ast = ast_rt,
           hfa = is_home, neutral = neutral_site, 
           rest = days_rest, miles = travel)

  
  # Build test data ------------------------
  test.data <- summary %>% 
    filter(game_date == test.date) %>% 
    select(rtg_act = {stat_est}, game_date, game_id, team_id, opp_id, is_home,
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
  
  rm(test.date, raw.coeff, base, hfa, tm_ratings, test.data,
     miles, neutral, rest)
  
  #Sys.sleep(0.5)
}

# Review results --------------
head(test.summary)

## Filter NAs
test.summary <- test.summary %>% 
  filter(!is.na(rtg_est))


data.diff <- test.summary %>% 
  left_join(test.summary %>% 
              select(game_id,
                     opp_id = team_id,
                     opp_act = rtg_act,
                     opp_est = rtg_est), by = c("game_id",
                                                "opp_id")) %>% 
  mutate(error = rtg_act - rtg_est,
         opp_error = opp_act - opp_est,
         act_diff = rtg_act - opp_act,
         est_diff = rtg_est - opp_est,
         total_error = act_diff - est_diff) %>% 
  select(game_date, team_id, opp_id,
         rtg_act, rtg_est, error,
         opp_act, opp_est,opp_error,
         act_diff, est_diff,
         total_error) %>% 
  mutate(error_pct = error / total_error)


head(data.diff)
cor(data.diff$error, data.diff$est_diff)

data.diff %>% mutate(est_diff = 1.3 * est_diff) %>%
  ggplot(aes(x = est_diff, y = act_diff)) + geom_point() + geom_smooth() +
  geom_abline(slope = 1, intercept = 0)

model <- glm(act_diff ~ est_diff, data = data.diff)


head(data.diff)
summary(model)







## Add results to data frame --------
#res <- data.frame(wgt = {wgt_var}, season = {season}, stat = {stat_name}, pred_stat = {stat_est},
#                  corr = round(c(cor(test.summary$rtg_act, test.summary$rtg_est)), 4),
#                  rmse = round(sqrt(mean((test.summary$rtg_act - test.summary$rtg_est)^2)), 4))

#print(res)

res <- res %>% 
  bind_rows(data.frame(wgt = as.numeric({wgt_var}), season = as.numeric({season}),
                       lambda = as.numeric({lambda_adj}),
                       stat = {stat_name}, pred_stat = {stat_est},
                       corr = as.numeric(round(c(cor(test.summary$rtg_act, test.summary$rtg_est)), 4)),
                       rmse = as.numeric(round(sqrt(mean((test.summary$rtg_act - test.summary$rtg_est)^2)), 4))))
print(res)

saveRDS(res, glue::glue("_Testing/Power Rating Weights/Results/single_season_{stat_name}_with_proj_res.rds"))


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

