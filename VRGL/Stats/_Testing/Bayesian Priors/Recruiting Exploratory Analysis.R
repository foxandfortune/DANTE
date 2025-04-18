library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

# Negate in formula
`%!in%` = Negate(`%in%`)


# Load Teams 
teams <- readRDS('Teams/team_database.rds')
teams247 <- readRDS('Teams/teams247.rds')


# Load Commits/Transfers/Draft Entrants
files.commits <- unlist(map(list.files(path = 'Recruiting', pattern = 'commits_'),
                            ~glue::glue("Recruiting/", ., sep = "")))

files.transfers <- unlist(map(list.files(path = 'Recruiting', pattern = 'transfers_'),
                              ~glue::glue("Recruiting/", ., sep = "")))

files.draftees <- unlist(map(list.files(path = 'Recruiting', pattern = 'draft_entrants_'),
                             ~glue::glue("Recruiting/", ., sep = "")))


commits <- map_df(files.commits, readRDS)
transfers <- map_df(files.transfers, readRDS)
draftees <- map_df(files.draftees, readRDS)


# Load rosters and player season stats -------------
files.rosters <- unlist(map(list.files(path = 'Rosters', pattern = 'roster_'),
                            ~glue::glue("Rosters/", ., sep = "")))

rosters.game <- map_df(files.rosters, readRDS)


## Clean names of rosters
rosters.game <- rosters.game %>% 
  mutate(match_name = nflreadr::clean_player_names(athlete_display_name),
         match_name = str_to_title(match_name)) %>% 
  mutate(match_name = case_when(
    str_detect(match_name, "William ") ~ str_replace(match_name, "William ", "Will "),
    str_detect(match_name, "Robert ") ~ str_replace(match_name, "Robert ", "Rob "),
    str_detect(match_name, "Cameron ") ~ str_replace(match_name, "Cameron ", "Cam "),
    str_detect(match_name, "Camron ") ~ str_replace(match_name, "Camron ", "Cam "),
    str_detect(match_name, "Joshua ") ~ str_replace(match_name, "Joshua ", "Josh "),
    str_detect(match_name, "Nicholas ") ~ str_replace(match_name, "Nicholas ", "Nick "),
    str_detect(match_name, "Nicolas ") ~ str_replace(match_name, "Nicolas ", "Nick "),
    str_detect(match_name, "Matthew ") ~ str_replace(match_name, "Matthew ", "Matt "),
    TRUE ~ match_name
  ))




# Get minimums for commit/transfer ratings by year
cmt_min <- commits %>% 
  filter(!is.na(player)) %>% 
  with_groups(.groups = c(season, pos),
              summarise,
              min_rtg = min(rating, na.rm = TRUE))

transf_min <- transfers %>%
  filter(season > 2019) %>% 
  with_groups(.groups = c(season),
              summarise,
              min_rtg = min(rating, na.rm = TRUE))


# Clean up some stuff
commits <- commits %>% 
  filter(cmttoURL != 'https://247sports.com/',
         !is.na(player)) %>% 
  left_join(cmt_min, by = c("season", "pos")) %>% 
  mutate(rating = case_when(
    !is.na(rating) ~ rating,
    is.na(rating) ~ min_rtg - 5)) %>% 
  select(-c(min_rtg))

transfers <- transfers %>% 
  filter(transto != 'https://247sports.com/',
         season > 2019,
         !is.na(player)) %>% 
  left_join(transf_min, by = "season") %>% 
  mutate(rating = case_when(
    !is.na(rating) ~ rating,
    is.na(rating) ~ min_rtg - 0.05))

# Graph distribution of commits
commits %>% 
  with_groups(.groups = season, mutate,
              percentile = cume_dist(rating)) %>% 
  ggplot(aes(x = rating)) + geom_histogram()+
  facet_grid(cols = vars(season))

## Look at distribution of "Class"
commits %>% 
  with_groups(.groups = season, mutate,
              percentile = cume_dist(rating)) %>%
  mutate(class = case_when(
    percentile >= .995 ~ 1,
    percentile >= .95 ~ 2,
    percentile >= .8 ~ 3,
    percentile >= .50 ~ 4,
    percentile < .50 ~ 5)) %>% 
  summarise(n = n(), .by = c(season, class)) %>% 
  with_groups(.groups = season, mutate,
              pct = 100 * round(n / sum(n), 2)) %>% 
  arrange(season, class)


# Graph distribution of transfers 
transfers %>% 
  with_groups(.groups = season, mutate,
              percentile = cume_dist(rating)) %>% 
  ggplot(aes(x = rating)) + geom_histogram()+
  facet_grid(cols = vars(season))

## Look at distribution of ratings
transfers %>% 
  filter(rating > min_rtg) %>% 
  with_groups(.groups = season, mutate,
              percentile = cume_dist(rating)) %>%
  mutate(class = case_when(
    percentile >= .995 ~ 1,
    percentile >= .95 ~ 2,
    percentile >= .6 ~ 3,
    percentile < .60 ~ 4)) %>% 
  summarise(n = n(), .by = c(season, class)) %>% 
  with_groups(.groups = season, mutate,
              pct = 100 * round(n / sum(n), 2)) %>% 
  arrange(season, class)
