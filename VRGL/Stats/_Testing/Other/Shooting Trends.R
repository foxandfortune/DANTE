library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

season <- 2024

# Load teams and summary data -----
teams <- readRDS(glue::glue("Teams/team_database.rds"))

summary <- readRDS(glue::glue("Power Ratings/Raw Data/poss_stats_with_types_{season}.rds"))

glimpse(summary)


# Look at points versus expected points trends over time ------------
## Points at rim ------------------------
summary %>% 
  arrange(game_date) %>% 
  with_groups(.groups = team_id,
              mutate,
              total_pts = cumsum(pts_rim),
              total_xpts = cumsum(xpts_rim)) %>% 
  with_groups(.groups = c(game_date, team_id),
              summarise,
              total_pts = sum(total_pts),
              total_xpts = sum(total_xpts),
              conv = total_pts / total_xpts) %>%
  with_groups(.groups = team_id,
              mutate,
              game_no = row_number()) %>% 
  arrange(team_id, game_date) %>% 
  ggplot(aes(x = game_no, y = conv, color = factor(team_id))) + geom_line() +
  theme(legend.position = "none")

## Non-rim twos ------------------------
summary %>% 
  arrange(game_date) %>% 
  with_groups(.groups = team_id,
              mutate,
              total_pts = cumsum(pts_nonrim),
              total_xpts = cumsum(xpts_nonrim)) %>% 
  with_groups(.groups = c(game_date, team_id),
              summarise,
              total_pts = sum(total_pts),
              total_xpts = sum(total_xpts),
              conv = total_pts / total_xpts) %>%
  with_groups(.groups = team_id,
              mutate,
              game_no = row_number()) %>% 
  arrange(team_id, game_date) %>% 
  ggplot(aes(x = game_no, y = conv, color = factor(team_id))) + geom_line() +
  theme(legend.position = "none")

## Three pointers ------------------------
summary %>% 
  arrange(game_date) %>% 
  with_groups(.groups = team_id,
              mutate,
              total_pts = cumsum(pts_three),
              total_xpts = cumsum(xpts_three)) %>% 
  with_groups(.groups = c(game_date, team_id),
              summarise,
              total_pts = sum(total_pts),
              total_xpts = sum(total_xpts),
              conv = total_pts / total_xpts) %>%
  with_groups(.groups = team_id,
              mutate,
              game_no = row_number()) %>% 
  arrange(team_id, game_date) %>% 
  ggplot(aes(x = game_no, y = conv, color = factor(team_id))) + geom_line() +
  theme(legend.position = "none")


## Overall points -------------
### Offense ----------------
summary %>% 
  arrange(game_date) %>% 
  with_groups(.groups = team_id,
              mutate,
              total_pts = cumsum(team_score),
              total_xpts = cumsum(xpoints)) %>% 
  with_groups(.groups = c(game_date, team_id),
              summarise,
              total_pts = sum(total_pts),
              total_xpts = sum(total_xpts),
              conv = total_pts / total_xpts) %>%
  with_groups(.groups = team_id,
              mutate,
              game_no = row_number()) %>% 
  arrange(team_id, game_date) %>% 
  filter(game_no <= 30) %>% 
  ggplot(aes(x = game_no, y = conv, color = factor(team_id))) + geom_line() +
  theme(legend.position = "none")

### Defense ---------------
summary %>% 
  arrange(game_date) %>% 
  with_groups(.groups = opponent_team_id,
              mutate,
              total_pts = cumsum(team_score),
              total_xpts = cumsum(xpoints)) %>% 
  with_groups(.groups = c(game_date, opponent_team_id),
              summarise,
              total_pts = sum(total_pts),
              total_xpts = sum(total_xpts),
              conv = total_pts / total_xpts) %>%
  with_groups(.groups = opponent_team_id,
              mutate,
              game_no = row_number()) %>% 
  arrange(opponent_team_id, game_date) %>% 
  ggplot(aes(x = game_no, y = conv, color = factor(opponent_team_id))) + geom_line() +
  theme(legend.position = "none")

  
## Compare variance for teams based on shot profile ------
glimpse(summary)

team_var <- summary %>%
  mutate(conv = team_score / xpoints,
         rim_rt = xpts_rim / xpoints,
         nonrim_rt = xpts_nonrim / xpoints,
         three_rt = xpts_three / xpoints) %>% 
  with_groups(.groups = team_id,
              summarise,
              games = n_distinct(game_id),
              rim_rt = mean(rim_rt, na.rm = TRUE),
              nonrim_rt = mean(nonrim_rt, na.rm = TRUE),
              three_rt = mean(three_rt, na.rm = TRUE),
              sd_conv = sd(conv, na.rm = TRUE)) %>% 
  filter(!is.na(sd_conv))


summary %>% 
  mutate(conv = team_score / xpoints) %>%
  summarise(sd(conv, na.rm = TRUE))

test <- glm(sd_conv ~ rim_rt + nonrim_rt + three_rt, 
            data = team_var)

summary(test)
team_var %>%
  ggplot(aes(x = rim_rt, y = sd_conv)) + geom_point()

cor(team_var$sd_conv, team_var$rim_rt)

team_var %>% 
  arrange(desc(sd_conv))

teams %>% 
  filter(team_id %in% c(2520, 2617, 2458, 233))
