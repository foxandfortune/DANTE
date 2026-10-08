library(rvest)
#library(hoopR)
library(tidyverse)

# Set working directory --------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set season and create URL --------
season <- 2026
url <- "https://www.hoopsrumors.com/2026/06/nba-announces-final-list-of-2026-draft-early-entrants.html"
#url <- 'https://www.hoopsrumors.com/2021/07/official-early-entrants-list-for-{season}-nba-draft.html'

# Scrape draft entrants ----------
## Hoops Rumors moved post text from .userContent to .entry-content.
## Each entrant is a list item like "Darius Acuff, G, Arkansas (freshman)".
draft_list <- read_html(url) %>%
  html_elements(".entry-content li") %>%
  html_text2() %>%
  str_squish() %>%
  tibble(player = .) %>%
  filter(str_count(player, ",") >= 2)   # keep only "name, pos, school" lines

if (nrow(draft_list) == 0) {
  stop("No entrants found - check the page layout / CSS selector.")
}

## Clean columns
draft_entrants <- draft_list %>%
  # Filter out foreign players/non NCAA
  filter(!str_detect(player, "\\(born")) %>%
  mutate(year = case_when(
    str_detect(player, "freshman") ~ "freshman",
    str_detect(player, "sophomore") ~ "sophomore",
    str_detect(player, "junior") ~ "junior",
    str_detect(player, "senior") ~ "senior",
    TRUE ~ "N/A")) %>%
  # Split to player/position/school, year (fr/so/jr/sr)
  separate_wider_delim(cols = player, delim = ",", names = c("player", "pos", "school"),
                       too_many = "merge") %>%
  mutate(player = str_trim(player),
         pos = str_trim(pos),
         school = gsub("\\s*\\([^\\)]+\\)", "", school),
         school = str_trim(school, side = "both"),
         season = {season})

print(draft_entrants, n = Inf)

# Save -------
saveRDS(draft_entrants, glue::glue("Recruiting - MBB/draft_entrants_{season}.rds"))
