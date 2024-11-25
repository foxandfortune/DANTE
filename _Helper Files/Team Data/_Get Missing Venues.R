library(geonames)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

# Negate in formula
`%!in%` = Negate(`%in%`)

# Set username in geonames
options(geonamesUsername="ballfromgrace")

# Load venues database
venues <- readRDS("team_venues.rds") %>% 
  bind_rows(readRDS("neutral_sites.rds")) %>% 
  distinct()

# Get schedule
schedule <- hoopR::load_mbb_schedule(seasons = c(2025))

missing_venues <- schedule %>% 
  filter(venue_id %!in% venues$venue_id, !is.na(venue_id)) %>% 
  select(venue_id:venue_indoor) %>% 
  distinct()

schedule %>% 
  filter(venue_id %!in% venues$venue_id, !is.na(venue_id), status_type_completed == TRUE) %>% 
  select(venue_id, home_id, home_display_name) %>% 
  distinct()


# Select unique city/state
locations <- missing_venues %>% 
  left_join(venues %>% 
              select(venue_address_city, venue_address_state, latitude:tz_offset) %>% 
              distinct(), by = c("venue_address_city", "venue_address_state")) %>% 
  filter(is.na(latitude),
         venue_address_state %!in% c("PQ", "Cayman Islands", "NB",
                                     "Puerto Rico")) %>% 
  select(venue_address_city, venue_address_state) %>% 
  distinct()

# Loop through locations to get coordinates
coord <- data.frame()

for(i in 1:length(locations$venue_address_city)){
  temp.coord <- GNsearch(name = locations$venue_address_city[i], country = "US") %>%
    #slice(1) %>% select(lat, lng)
    filter(str_detect(fclName, "city"),
           name == locations$venue_address_city[i],
           adminCode1 == locations$venue_address_state[i]) %>% 
    mutate(max_pop = max(population)) %>% 
    filter(population == max_pop) %>% 
    select(lat, lng)
  
  temp.tz <- GNtimezone(lat = temp.coord$lat, lng = temp.coord$lng)
  
  Sys.sleep(2)
  
  temp.df <- data.frame(city = locations$venue_address_city[i],
                        state = locations$venue_address_state[i],
                        latitude = temp.coord$lat,
                        longitude = temp.coord$lng,
                        time_zone = temp.tz$timezoneId,
                        tz_offset = (temp.tz$rawOffset + 5))
  
  coord <- rbind(coord, temp.df)
  
  if(i < length(locations$venue_address_city)){
    Sys.sleep(3)
  }
  
  print(i)
}


coord <- coord %>% 
  distinct() %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))


# Add to venues
missing_venues <- missing_venues %>% 
  left_join(venues %>% 
              select(venue_address_city, venue_address_state, latitude:tz_offset) %>% 
              distinct(), by = c("venue_address_city", "venue_address_state")) %>% 
  filter(!is.na(latitude)) %>% 
  bind_rows(missing_venues %>% 
          left_join(venues %>% 
                      select(venue_address_city, venue_address_state, latitude:tz_offset) %>% 
                      distinct(), by = c("venue_address_city", "venue_address_state")) %>% 
          filter(is.na(latitude)) %>% 
          select(-c(latitude:tz_offset)) %>% 
          left_join(coord, by = c("venue_address_city" = "city", "venue_address_state" = "state")) %>% 
          filter(!is.na(latitude)))

missing_venues <- missing_venues %>% 
  left_join(schedule %>% 
              filter(neutral_site == FALSE, venue_id %in% missing_venues$venue_id,
                     status_type_completed == TRUE) %>% 
              select(venue_id, home_id) %>% 
              distinct(), by = "venue_id") %>% 
  rename(team_id = home_id) %>% 
  relocate(team_id)



team_venues <- venues %>% 
  bind_rows(missing_venues %>% 
              mutate(team_id = as.character(team_id))) %>% 
  filter(!is.na(team_id))

neutral_sites <- venues %>% 
  bind_rows(missing_venues %>% 
              mutate(team_id = as.character(team_id))) %>% 
  filter(is.na(team_id))


saveRDS(team_venues, "team_venues.rds")
saveRDS(neutral_sites, "neutral_sites.rds")

schedule %>% 
  filter(venue_id %!in% team_venues$venue_id & 
           venue_id %!in% missing_venues$venue_id) %>% 
  select(venue_address_city, venue_address_state) %>% 
  distinct()
