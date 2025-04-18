library(wehoop)
library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

# Negate in formula
`%!in%` = Negate(`%in%`)

# Set season ---------
cruit_yr <- 2021

# Load Teams 
teams <- readRDS('Teams/team_database_wbb.rds')

# Load rosters and player season stats -------------
rosters.game <- wehoop::load_wbb_player_box(seasons = {cruit_yr})
wehoop::espn_wbb_game_rosters(game_id = 401311269) %>% 
  select(contains('status'))
names(rosters.game)

rosters.game <- cbbdata::cbd_torvik_player_season(year = {cruit_yr}) %>% 
  as.data.frame() %>% 
  mutate(match_name = nflreadr::clean_player_names(player),
         match_name = str_to_title(match_name),
         match_name = str_remove_all(match_name, "Jr "),
         match_name = str_remove_all(match_name, " Jr")) %>%
  left_join(all_teams, by = c("team" = "torvik_team"))

# Get minimums for commit/transfer ratings by year
cmt_min <- commits %>% 
  filter(!is.na(player)) %>% 
  with_groups(.groups = c(season, pos),
              summarise,
              min_rtg = min(rating, na.rm = TRUE))

transf_min <- transfers %>%
  with_groups(.groups = c(season),
              summarise,
              min_rtg = min(rating, na.rm = TRUE))

#transf_min$min_rtg <- 0.85

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
  
## Look at distribution of "Class" -------------------
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

## If good, add class to commits -------------------
commits <- commits %>% 
  with_groups(.groups = season, mutate,
              percentile = cume_dist(rating)) %>%
  mutate(class = case_when(
    percentile >= .995 ~ 1,
    percentile >= .95 ~ 2,
    percentile >= .8 ~ 3,
    percentile >= .50 ~ 4,
    percentile < .50 ~ 5)) %>%
  # Fix an issue with Sharks/Blackbirds
  mutate(cmtto = case_when(
    cmtto == "Long Island Blackbirds" ~ "LIU Sharks",
    TRUE ~ cmtto)) %>% 
  left_join(all_teams %>% 
              select(Team, team_id), by = c("cmtto" = "Team"))

# Look at it
head(commits)

unique(commits$pos)

## Save adjusted commits
saveRDS(commits, glue::glue('Recruiting/commits_clean_{cruit_yr}.rds'))

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

## If good, add to transfers
transfers <- transfers %>% 
  filter(rating > min_rtg) %>% 
  with_groups(.groups = season, mutate,
              percentile = cume_dist(rating)) %>%
  mutate(class = case_when(
    percentile >= .995 ~ 1,
    percentile >= .95 ~ 2,
    percentile >= .6 ~ 3,
    percentile < .60 ~ 4)) %>% 
  bind_rows(transfers %>% 
              filter(rating <= min_rtg) %>% 
              mutate(class = 5,
                     rating = transf_min$min_rtg - 0.05))

## Get player names ----------
### Loop through until there are no more discrepancies ------------
transfers %>% 
  mutate(match_name = str_trim(nflreadr::clean_player_names(player)),
         match_name = str_to_title(match_name),
         match_name = str_remove_all(match_name, '\''),
         match_name = str_remove_all(match_name, "Jr "),
         match_name = str_remove_all(match_name, " Jr")) %>%
  # Start fixing names
  mutate(match_name = case_when(
    str_detect(match_name, "Monty Scott") ~ "James Scott",
    str_detect(match_name, " Japhet") ~ "Sam Japhet-Mathias",
    TRUE ~ match_name
  )) %>% 
  filter(match_name %!in% rosters.game$match_name) %>%
  select(match_name, transfrom, transto, season) %>% 
  arrange(match_name)

  
rosters.game %>% 
  filter(str_detect(match_name, " Japhet")) %>%  
  select(match_name, Team) %>% 
  arrange(match_name)

### Double check positions ----------
transfers %>%
  filter(pos %!in% c("PF", "CG", "PG",
                     "SF", "SG", "C"))

## Add cleaned names to transfers 
transfers_clean <- transfers %>% 
  # Add Transfer From Team to match ESPN
  left_join(all_teams, by = c("transfromURL" = "URL")) %>% 
  rename(transfromTeam = Team,
         transfromID = team_id, 
         transfromTorvik = torvik_team) %>% 
  # Add Transfer To Team to match ESPN
  left_join(all_teams, by = c("transtoURL" = "URL")) %>% 
  rename(transtoTeam = Team,
         transtoID = team_id, 
         transtoTorvik = torvik_team) %>% 
  # Clean names
  mutate(match_name = str_trim(nflreadr::clean_player_names(player)),
         match_name = str_to_title(match_name),
         match_name = str_remove_all(match_name, '\''),
         match_name = str_remove_all(match_name, "Jr "),
         match_name = str_remove_all(match_name, " Jr")) %>%
  # Start fixing names
  mutate(match_name = case_when(
    str_detect(match_name, "Monty Scott") ~ "James Scott",
    str_detect(match_name, " Japhet") ~ "Sam Japhet-Mathias",
    TRUE ~ match_name
  )) %>% 
  # Fix player positions
  mutate(pos = case_when(
    pos == "TE" ~ "PF",
    pos == "IOL" ~ "C",
    pos == "QB" & player == "Drew Carter" ~ "SG",
    pos == "QB" & player == "Conrad Hawley" ~ "PF",
    TRUE ~ pos)) %>% 
  distinct()

## Check again
transfers_clean %>% 
  filter(match_name %!in% rosters.game$match_name)

head(transfers_clean)

transfers_clean %>% filter(is.na(pos))

## Add player minutes to transfer from -------------------------
head(rosters.game)

# Stats: Per game (minutes, points, oreb/dreb, assits, TO);
# Rates: efg; off/def box plus minus; oreb/dreb; assist; turnover; steal
transfers_clean <- transfers_clean %>%
  left_join(rosters.game %>% 
              select(match_name, Team, g, mpg,
                     ppg, oreb, dreb, apg, tov, efg,
                     obpm, dbpm, oreb_rate, dreb_rate,
                     ast, to, stl),
            by = c("match_name", "transfromTeam" = "Team")) %>% 
  mutate(total_min = replace_na(mpg * g, 0)) %>% 
  mutate_at(c('g', 'mpg', 'ppg', 'oreb', 'dreb', 'apg', 'tov', 'efg',
              'obpm', 'dbpm', 'oreb_rate', 'dreb_rate',
              'ast', 'to', 'stl', 'total_min'), ~replace_na(., 0))

head(transfers_clean)

## Save file -----------
saveRDS(transfers_clean, glue::glue('Recruiting/transfers_clean_{cruit_yr}.rds'))

# Look at draft entrants -----------------------------
## Clean up school names -----------------
missing_schools <- draftees %>% 
  mutate(school = str_trim(school)) %>% 
  mutate(school = case_when(
    school == "Detroit" ~ "Detroit Mercy",
    school == "St. Joseph’s" ~ "Saint Joseph's",
    school == "UNC" ~ "North Carolina",
    school == "Saint Mary’s" ~ "Saint Mary's",
    school == "Saint Joseph’s" ~ "Saint Joseph's",
    school == "UC-Riverside" ~ "UC Riverside",
    school == "N.C. State" ~ "NC State",
    school == "North Carolina State" ~ "NC State",
    school == "Hawaii" ~ "Hawai'i",
    school == "UCSB" ~ "UC Santa Barbara",
    school == "Santa Barbara" ~ "UC Santa Barbara",
    school == "UMass-Lowell" ~ "UMass Lowell",
    school == "Mississippi" ~ "Ole Miss",
    school == "San Jose State" ~ "San José State",
    school == "Connecticut" ~ "UConn",
    school == "Bowling Green State" ~ "Bowling Green",
    school == "Pitt" ~ "Pittsburgh",
    TRUE ~ school)) %>% 
  filter(school %!in% teams$team,
         !str_detect(school, " CC"),
         school != "IMG Academy") %>% 
  select(school) %>% 
  distinct()

print(missing_schools)


draftees %>% 
  mutate(school = case_when(
    school == "Detroit" ~ "Detroit Mercy",
    school == "St. Joseph’s" ~ "Saint Joseph's",
    school == "UNC" ~ "North Carolina",
    school == "Saint Mary’s" ~ "Saint Mary's",
    school == "Saint Joseph’s" ~ "Saint Joseph's",
    school == "UC-Riverside" ~ "UC Riverside",
    school == "N.C. State" ~ "NC State",
    school == "North Carolina State" ~ "NC State",
    school == "Hawaii" ~ "Hawai'i",
    school == "UCSB" ~ "UC Santa Barbara",
    school == "Santa Barbara" ~ "UC Santa Barbara",
    school == "UMass-Lowell" ~ "UMass Lowell",
    school == "Mississippi" ~ "Ole Miss",
    school == "San Jose State" ~ "San José State",
    school == "Connecticut" ~ "UConn",
    school == "Bowling Green State" ~ "Bowling Green",
    school == "Pitt" ~ "Pittsburgh",
    TRUE ~ school)) %>%
  filter(school %!in% teams$team,
         !str_detect(school, " CC"),
         school != "IMG Academy") %>% 
  select(school) %>% 
  distinct() %>% 
  print(n = 44)

## Clean player names -----------
draftees %>% 
  mutate(match_name = nflreadr::clean_player_names(player),
         match_name = str_to_title(match_name),
         match_name = str_remove_all(match_name, "Jr "),
         match_name = str_remove_all(match_name, " Jr"),
         match_name = str_remove_all(match_name, '\'')) %>% 
  filter(match_name %!in% rosters.game$match_name,
         school %!in% missing_schools$school)

rosters.game %>% 
  filter(str_detect(match_name, "Ayinde")) %>% 
  select(match_name, team) %>% 
  arrange(match_name)

## Save changes into data frame ---------
draftees_clean <- draftees %>%
  mutate(match_name = nflreadr::clean_player_names(player),
         match_name = str_to_title(match_name),
         match_name = str_remove_all(match_name, "Jr "),
         match_name = str_remove_all(match_name, " Jr"),
         match_name = str_remove_all(match_name, '\'')) %>% 
  mutate(school = case_when(
    school == "Detroit" ~ "Detroit Mercy",
    school == "St. Joseph’s" ~ "Saint Joseph's",
    school == "UNC" ~ "North Carolina",
    school == "Saint Mary’s" ~ "Saint Mary's",
    school == "Saint Joseph’s" ~ "Saint Joseph's",
    school == "UC-Riverside" ~ "UC Riverside",
    school == "N.C. State" ~ "NC State",
    school == "North Carolina State" ~ "NC State",
    school == "Hawaii" ~ "Hawai'i",
    school == "UCSB" ~ "UC Santa Barbara",
    school == "Santa Barbara" ~ "UC Santa Barbara",
    school == "UMass-Lowell" ~ "UMass Lowell",
    school == "Mississippi" ~ "Ole Miss",
    school == "San Jose State" ~ "San José State",
    school == "Connecticut" ~ "UConn",
    school == "Bowling Green State" ~ "Bowling Green",
    school == "Pitt" ~ "Pittsburgh",
    TRUE ~ school),
    match_name = case_when(
      match_name == "Simisola Shittu" ~ "Simi Shittu",
      TRUE ~ match_name
    )) %>%
  filter(school %in% teams$team)

head(draftees_clean)

# Add stats to draftees 
draftees_clean <- draftees_clean %>% 
  left_join(teams %>% 
              select(team_id, team), by = c("school" = "team")) %>%
  left_join(rosters.game %>% 
              select(match_name, team_id, g, mpg,
                     ppg, oreb, dreb, apg, tov, efg,
                     obpm, dbpm, oreb_rate, dreb_rate,
                     ast, to, stl),
            by = c("match_name", "team_id")) %>% 
  mutate(total_min = replace_na(mpg * g, 0)) %>% 
  mutate_at(c('g', 'mpg', 'ppg', 'oreb', 'dreb', 'apg', 'tov', 'efg',
              'obpm', 'dbpm', 'oreb_rate', 'dreb_rate',
              'ast', 'to', 'stl', 'total_min'), ~replace_na(., 0))

head(draftees_clean)

unique(draftees_clean$pos)

## Save file
saveRDS(draftees_clean, glue::glue('Recruiting/draft_entrants_clean_{cruit_yr}.rds'))

# Get seniors that did not enter draft ----------------------------
graduates <- rosters.game %>%
  filter(match_name %!in% transfers_clean$match_name,
         match_name %!in% draftees_clean$match_name,
         exp == "Sr") %>% 
  select(player, match_name, team_id, Team, pos, season = year,
         g, mpg,
         ppg, oreb, dreb, apg, tov, efg,
         obpm, dbpm, oreb_rate, dreb_rate,
         ast, to, stl) %>% 
  mutate(total_min = replace_na(mpg * g, 0)) %>% 
  mutate_at(c('g', 'mpg', 'ppg', 'oreb', 'dreb', 'apg', 'tov', 'efg',
              'obpm', 'dbpm', 'oreb_rate', 'dreb_rate',
              'ast', 'to', 'stl', 'total_min'), ~replace_na(., 0))

head(graduates)

#graduates <- graduates %>% mutate(pos = case_when(is.na(pos) ~ "Combo G", TRUE ~ pos))

graduates %>% 
  filter(is.na(pos))

unique(graduates$pos)

## Save graduating seniors ----------------
saveRDS(graduates, glue::glue('Recruiting/graduates_clean_{cruit_yr}.rds'))


