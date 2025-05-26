library(tidyverse)
library(rvest)
library(httr)

`%!in%` = Negate(`%in%`)

# Load conference IDs ----
conf_ids <- read.csv('cbb_conference_ids.csv')

# Set season --------
season <- 2021

## Filter conference IDs ------
if(season > 2024){
  conf_to_scrape <- conf_ids %>% 
    filter(
      conf_name != 'Pac-12 Conference',
      conf_name != 'Independent'
    )
} else if(season < 2023) {
  conf_to_scrape <- conf_ids %>% 
    filter(
      conf_name != 'Independent'
    )
} else {
  conf_to_scrape <- conf_ids
}

mbb_conf <- data.frame()
wbb_conf <- data.frame()


for(i in 1:length(conf_to_scrape$conf_id)){
  conf_id.temp <- conf_to_scrape$conf_id[i]
  
  url_mbb <- glue::glue("https://www.espn.com/mens-college-basketball/standings/_/season/{season}/group/{conf_id.temp}")
  url_wbb <- glue::glue("https://www.espn.com/womens-college-basketball/standings/_/season/{season}/group/{conf_id.temp}")
  
  mbb.temp <- read_html(url_mbb) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.data.frame() %>% 
    filter(str_detect(., 'team/_/id/')) %>% 
    distinct() %>% 
    mutate(team_id = str_remove_all(., '.*id/'),
           team_id = str_remove_all(team_id, '\\/.*')) %>%
    select(team_id) %>% 
    mutate(season = {season},
           conf_id = conf_id.temp)
  
  Sys.sleep(5)
  
  wbb.temp <- read_html(url_wbb) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.data.frame() %>% 
    filter(str_detect(., 'team/_/id/')) %>% 
    distinct() %>% 
    mutate(team_id = str_remove_all(., '.*id/'),
           team_id = str_remove_all(team_id, '\\/.*')) %>%
    select(team_id) %>% 
    mutate(season = {season},
           conf_id = conf_id.temp)
  
  mbb_conf <- bind_rows(mbb_conf,
                        mbb.temp)
  
  wbb_conf <- bind_rows(wbb_conf,
                        wbb.temp)
  
  print(i)
  
  Sys.sleep(5)
  
}

all_mbb <- read_html(glue::glue("https://www.espn.com/mens-college-basketball/standings/_/season/{season}/")) %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  as.data.frame() %>% 
  filter(str_detect(., 'team/_/id/')) %>% 
  distinct() %>% 
  mutate(team_id = str_remove_all(., '.*id/'),
         team_id = str_remove_all(team_id, '\\/.*')) %>%
  select(team_id)

all_wbb <- read_html(glue::glue("https://www.espn.com/womens-college-basketball/standings/_/season/{season}/")) %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  as.data.frame() %>% 
  filter(str_detect(., 'team/_/id/')) %>% 
  distinct() %>% 
  mutate(team_id = str_remove_all(., '.*id/'),
         team_id = str_remove_all(team_id, '\\/.*')) %>%
  select(team_id)

all_mbb %>% 
  filter(team_id %!in% mbb_conf$team_id)

all_wbb %>% 
  filter(team_id %!in% wbb_conf$team_id)

wbb_conf %>%
  with_groups(.groups = conf_id,
              summarise,
              n = n()) %>% 
  left_join(conf_ids, by = "conf_id") %>% 
  arrange(conference_short_name)

missing <- all_wbb %>% 
  filter(team_id %!in% wbb_conf$team_id) %>% 
  bind_cols(data.frame(season = rep(2021, 44),
                       conf_id = c(rep(11, 14),
                                   rep(16, 8),
                                   rep(22, 10),
                                   rep(27, 12))))

wbb_conf <- wbb_conf %>% 
  bind_rows(wbb_conf_2023 %>% filter(conf_id == 12) %>% mutate(season = 2021))
  bind_rows(missing %>% 
              mutate(team_id = as.character(team_id)))


saveRDS(mbb_conf, glue::glue('CBB/Conferences/mbb_conf_{season}.rds'))
saveRDS(wbb_conf, glue::glue('CBB/Conferences/wbb_conf_{season}.rds'))

