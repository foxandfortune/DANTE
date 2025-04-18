library(rvest)
library(hoopR)
library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load teams
teams.247 <- readRDS('Teams/teams247.rds')

# Scrape transfers by season --------------------
## Set season -----
season <- 2018

## Create transfer data.frame ------
transfer.df <- data.frame()
commit.df <- data.frame()

## Loop through team URLs to get players -----
for(i in 1:length(teams.247$URL)){
  temp.URL <- glue::glue("{teams.247$URL[i]}season/{season}-basketball/commits/")
  rmv <- str_replace_all(str_to_lower(temp.URL), 'commits', 'scholarshipdistribution')
  
  temp.page <- read_html(temp.URL)
  
  # Set portal
  temp.portal <- temp.URL %>% 
    read_html() %>% 
    html_elements('.portal-list_itm')
  
  ### Transfers -------
  #### Player name
  temp.plyr.tr <- temp.portal %>%
    html_element('.player') %>%
    html_text2() %>%
    str_replace_all(pattern = c("\n.*" = "")) %>% 
    as_tibble() %>% 
    as.data.frame() %>% 
    rename(player = value) %>% 
    slice(2:n())
  
  #### Positions
  temp.pos.tr <- temp.portal %>% 
    html_element('.position') %>% 
    html_text2() %>% 
    as_tibble() %>% 
    as.data.frame() %>% 
    rename(pos = value) %>% 
    slice(2:n())
  
  #### URLs
  temp.transfURL <- temp.portal %>%
    html_element('.transfer-institution a') %>%
    html_attr("href") %>%
    str_remove_all(pattern = rmv) %>% 
    str_remove_all(pattern = 'season.*') %>% 
    as_tibble() %>% 
    as.data.frame() %>% 
    rename(transfromURL = value) %>% 
    slice(2:n())
  
  #### Team name for transfer from
  temp.tmname.tr <- temp.portal %>%
    html_element('.transfer-institution a') %>%
    html_attr("href") %>%
    str_remove_all(pattern = rmv) %>% 
    str_replace_all(pattern = c("https://247sports.com/college/" = "")) %>% 
    str_remove_all(pattern = c('\\/.*')) %>% 
    str_replace_all("-", " ") %>% 
    str_to_title() %>% 
    as_tibble() %>% 
    as.data.frame() %>% 
    rename(transfrom = value) %>% 
    slice(2:n()) 
    
  #### Player rating
  temp.rating.tr <- temp.portal %>%
    html_elements('.player') %>%
    html_element(".score") %>%
    html_text2() %>%
    str_replace_all(pattern = c(" .(T.)" = "")) %>%
    as.double() %>% 
    as_tibble() %>% 
    as.data.frame() %>% 
    rename(rating = value)
    
  #### Make into data frame
  temp.df.tr <- bind_cols(temp.plyr.tr,
                          temp.pos.tr,
                          temp.rating.tr,
                          temp.tmname.tr,
                          temp.transfURL)
  
  #### Add columns for teams transferred to 
  if(length(temp.df.tr$player) > 0){temp.df.tr$transtoURL <- teams.247$URL[i]}
  if(length(temp.df.tr$player) > 0){temp.df.tr$transto <- teams.247$Team[i]}
  if(length(temp.df.tr$player) > 0){temp.df.tr$season <- {season}}
  
  #### Add to transfer list
  if(length(temp.df.tr$player) > 0){transfer.df <- rbind(transfer.df, temp.df.tr)}
  
  ### Commits -----
  # Set commit
  temp.commit <- temp.URL %>% 
    read_html() %>% 
    html_elements('.ri-page__list-item')
  
  #### Player name
  temp.plyr.cmt <- temp.commit %>% 
    html_element('.recruit a') %>% 
    html_text2() %>% 
    as_tibble() %>% 
    as.data.frame() %>% 
    rename(player = value) %>% 
    slice(2:n())
  
  #### Position
  temp.pos.cmt <- temp.commit %>% 
    html_element('.position') %>% 
    html_text2() %>% 
    as_tibble() %>% 
    as.data.frame() %>% 
    rename(pos = value) %>% 
    slice(2:n())
  
  #### Player rating
  temp.rating.cmt <- temp.commit %>%
    html_element('.rating') %>% 
    html_text2() %>% 
    str_replace_all(pattern = c("\n.*" = "")) %>% 
    as.double() %>% 
    as_tibble() %>% 
    as.data.frame() %>% 
    rename(rating = value) %>% 
    slice(2:n())
  
  #### Player rankings
  ##### National ranking
  temp.natrnk.cmt <- temp.commit %>% 
    html_element('.rank') %>% 
    html_text2() %>% 
    str_remove_all(pattern = c(" .*")) %>% 
    as.double() %>% 
    as_tibble() %>% 
    as.data.frame() %>% 
    rename(rank_overall = value) %>% 
    slice(2:n())
  
  ##### Positional ranking
  temp.posrnk.cmt <- temp.commit %>% 
    html_element('.rank') %>% 
    html_text2() %>% 
    str_remove(pattern = c(".*? ")) %>% 
    str_remove_all(pattern = c(" .*")) %>% 
    as.double() %>% 
    as_tibble() %>% 
    as.data.frame() %>% 
    rename(rank_pos = value) %>% 
    slice(2:n())
  
  #### Make into data frame
  temp.df.cmt <- bind_cols(temp.plyr.cmt,
                           temp.pos.cmt,
                           temp.natrnk.cmt,
                           temp.posrnk.cmt,
                           temp.rating.cmt)
  
  #### Add columns for team
  if(length(temp.df.cmt$player) > 0){temp.df.cmt$cmttoURL <- teams.247$URL[i]}
  if(length(temp.df.cmt$player) > 0){temp.df.cmt$cmtto <- teams.247$Team[i]}
  if(length(temp.df.cmt$player) > 0){temp.df.cmt$season <- {season}}
  
  #### Add to transfer list
  if(length(temp.df.cmt$player) > 0){{commit.df <- rbind(commit.df, temp.df.cmt)}}
  
  ### Regular loop update stuff
  print(glue::glue("{i} of {length(teams.247$URL)}"))
  
  if(i < length(teams.247$URL)){
    Sys.sleep(2.5)
  }
  
  if(i %% 10 == 0 | i == length(teams.247$URL)){
    saveRDS(transfer.df, glue::glue("Recruiting/transfers_{season}.rds"))
    saveRDS(commit.df, glue::glue("Recruiting/commits_{season}.rds"))
  }
  
  rm(temp.commit, temp.df.cmt, temp.df.tr,
     temp.natrnk.cmt, temp.page, temp.plyr.cmt, temp.plyr.tr,
     temp.portal, temp.pos.cmt, temp.pos.tr,
     temp.posrnk.cmt, temp.rating.cmt, temp.rating.tr,
     temp.transfURL, temp.tmname.tr, rmv, temp.URL)
  
}

saveRDS(transfer.df, glue::glue("Recruiting/transfers_{season}.rds"))
saveRDS(commit.df, glue::glue("Recruiting/commits_{season}.rds"))
