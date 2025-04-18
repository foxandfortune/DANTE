library(rvest)
#library(hoopR)
library(tidyverse)

# Set working directory --------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set season and create URL --------
season <- 2018
url <- glue::glue('https://www.hoopsrumors.com/{season}/06/official-early-entrants-list-for-{season}-nba-draft.html')
#url <- 'https://www.hoopsrumors.com/2021/07/official-early-entrants-list-for-{season}-nba-draft.html'

# Scrape draft entrants ----------
draft_list <- data.frame(player = read_html(url) %>%
                               html_nodes(".userContent li") %>%
                               html_text() %>% 
                               as.character())

## Clean columns
draft_entrants <- draft_list %>% 
  # Filter out foreign players/non NCAA
  filter(!str_detect(player, "\\(born"))%>% 
  mutate(year = case_when(
    str_detect(player, "freshman") ~ "freshman",
    str_detect(player, "sophomore") ~ "sophomore",
    str_detect(player, "junior") ~ "junior",
    str_detect(player, "N/A") ~ "N/A",
    TRUE ~ "N/A")) %>% 
  # Split to player/position/school, year (fr/so/jr/sr)
  separate_wider_delim(cols = player, delim = ",", names = c("player", "pos", "school")) %>% 
  mutate(pos = str_trim(pos, side = "left"),
         school = gsub("\\s*\\([^\\)]+\\)","", school),
         school = str_trim(school, side = "both")) %>% 
  mutate(season = {season},
         row = row_number(),
         year = case_when(
           row <= 73 ~ "senior",
           TRUE ~ year
         )) %>% 
  select(-row)

print(draft_entrants)

# Save -------
saveRDS(draft_entrants, glue::glue("Recruiting/draft_entrants_{season}.rds"))

