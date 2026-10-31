library(geonames)
library(tidyverse)

# Find the DANTE folder: from this script's location when it is open in RStudio,
# else from the working directory (Rscript, GitHub Actions). The DANTE project
# doesn't need to be open.
dante_root <- local({
  starts <- c(tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) ""), getwd())
  for (s in starts[nzchar(starts)]) {
    r <- tryCatch(rprojroot::find_root(rprojroot::has_file("DANTE.Rproj"), path = s), error = function(e) NULL)
    if (!is.null(r)) return(r)
  }
  stop("Can't find the DANTE folder (DANTE.Rproj): open the DANTE project or setwd() into it")
})
dante_path <- function(...) file.path(dante_root, ...)
setwd(dante_path("_Helper Files", "Team Data"))
#setwd('..')

# Negate in formula
`%!in%` = Negate(`%in%`)

# Set username in geonames
options(geonamesUsername="ballfromgrace")

# Settings ---------------------------------------------------------------------
# Locations already saved that should be looked up again ("City, State" as ESPN
# writes them). Cancun was saved with coordinates in central Mexico; this fixes it.
# Set to character(0) once it has been run.
redo_locations <- c("Cancun, Mexico")

# Countries for venues outside the US. ESPN puts the country, a Canadian province or
# a territory in venue_address_state. Add new ones here (GeoNames two-letter codes);
# anything not listed is searched worldwide and flagged for you to check.
country_codes <- c(
  # US territories
  "Puerto Rico" = "PR", "Virgin Islands" = "VI", "Guam" = "GU",
  # Caribbean, Mexico and Central America
  "Bahamas" = "BS", "Cayman Islands" = "KY", "Jamaica" = "JM", "Turks and Caicos" = "TC",
  "Dominican Republic" = "DO", "Aruba" = "AW", "Bermuda" = "BM", "Barbados" = "BB",
  "Mexico" = "MX", "Costa Rica" = "CR", "Panama" = "PA",
  # Canada (provinces as ESPN writes them)
  "Canada" = "CA", "AB" = "CA", "BC" = "CA", "MB" = "CA", "NB" = "CA", "NL" = "CA", "NS" = "CA",
  "ON" = "CA", "PE" = "CA", "PQ" = "CA", "QC" = "CA", "SK" = "CA",
  # Europe
  "England" = "GB", "Scotland" = "GB", "Wales" = "GB", "Northern Ireland" = "GB", "United Kingdom" = "GB",
  "Ireland" = "IE", "France" = "FR", "Germany" = "DE", "Italy" = "IT", "Spain" = "ES",
  "Portugal" = "PT", "Netherlands" = "NL", "Belgium" = "BE", "Greece" = "GR",
  # Elsewhere
  "China" = "CN", "Japan" = "JP", "South Korea" = "KR", "Philippines" = "PH",
  "Australia" = "AU", "Israel" = "IL", "United Arab Emirates" = "AE")

# Look up one city: coordinates, time zone, and tz_offset (hours from US Eastern
# standard time, as in the saved venues). US cities must match the state; others
# match the country. Prefers a populated place with the same name (accents ignored),
# then any place with that name, then the largest populated place GeoNames returns.
geocode_location <- function(city, state) {
  us <- state %in% c(state.abb, "DC")
  country <- if (us) "US" else unname(country_codes[state])
  res <- if (is.na(country)) GNsearch(name = city, maxRows = 50) else GNsearch(name = city, country = country, maxRows = 50)
  if (is.null(res) || nrow(res) == 0) stop("GeoNames found nothing")
  plain <- function(x) tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
  for (col in c("toponymName", "adminCode1", "adminName1", "countryName", "fclName", "population"))
    if (!col %in% names(res)) res[[col]] <- NA_character_
  res <- res %>%
    mutate(lat = as.numeric(lat), lng = as.numeric(lng),
           population = replace_na(as.numeric(population), 0),
           same_name = plain(name) == plain(city) | plain(toponymName) == plain(city))
  if (us) res <- res %>% filter(adminCode1 == state)
  if (is.na(country)) {
    in_country <- res %>% filter(plain(countryName) == plain(state))
    if (nrow(in_country) > 0) res <- in_country
  }
  pick <- res %>% filter(same_name, str_detect(coalesce(fclName, ""), "city"))
  if (nrow(pick) == 0) pick <- res %>% filter(same_name)
  if (nrow(pick) == 0 && !us) pick <- res %>% filter(str_detect(coalesce(fclName, ""), "city"))
  if (nrow(pick) == 0) stop("no match for the city name")
  pick <- pick %>% arrange(desc(population)) %>% slice(1)

  found <- c(pick$name, pick$adminName1, pick$countryName)
  tz <- GNtimezone(lat = pick$lat, lng = pick$lng)
  data.frame(city = city, state = state,
             latitude = pick$lat, longitude = pick$lng,
             time_zone = tz$timezoneId,
             tz_offset = as.numeric(tz$rawOffset) + 5,
             found = paste(found[!is.na(found) & nzchar(found)], collapse = ", "),
             check = !all(pick$same_name) || is.na(country))
}

# Load venues database
venues <- readRDS("team_venues_wbb.rds") %>%
  bind_rows(readRDS("neutral_sites_wbb.rds")) %>%
  distinct()

# Get schedule
source(dante_path("_Helper Files", "season.R"))
schedule <- wehoop::load_wbb_schedule(seasons = c(dante_season))
d1_teams <- as.character(readRDS(glue::glue("Conferences/wbb_conf_{dante_season}.rds"))$team_id)

missing_venues <- schedule %>%
  filter(venue_id %!in% venues$venue_id, !is.na(venue_id)) %>%
  select(venue_id:venue_indoor) %>%
  distinct()

schedule %>%
  filter(venue_id %!in% venues$venue_id, !is.na(venue_id), status_type_completed == TRUE) %>%
  select(venue_id, home_id, home_display_name) %>%
  distinct()


# Select unique city/state (US and abroad) not already saved, plus any redos
redo <- tibble(venue_address_city  = str_trim(str_remove(redo_locations, ",.*$")),
               venue_address_state = str_trim(str_remove(redo_locations, "^[^,]*,")))

locations <- missing_venues %>%
  left_join(venues %>%
              select(venue_address_city, venue_address_state, latitude:tz_offset) %>%
              distinct(), by = c("venue_address_city", "venue_address_state")) %>%
  filter(is.na(latitude),
         !is.na(venue_address_city), !is.na(venue_address_state)) %>%
  select(venue_address_city, venue_address_state) %>%
  bind_rows(redo) %>%
  distinct()

# Loop through locations to get coordinates
coord <- data.frame()
not_found <- character(0)

for(i in seq_len(nrow(locations))){
  city <- locations$venue_address_city[i]
  state <- locations$venue_address_state[i]
  temp.df <- tryCatch(geocode_location(city, state),
                      error = function(e) { message(city, ", ", state, ": ", conditionMessage(e)); NULL })

  if (is.null(temp.df)) {
    not_found <- c(not_found, paste0(city, ", ", state))
  } else {
    coord <- rbind(coord, temp.df)
    cat(sprintf("%d/%d  %s, %s -> %s (%.3f, %.3f, %s)%s\n", i, nrow(locations), city, state,
                temp.df$found, temp.df$latitude, temp.df$longitude, temp.df$time_zone,
                if (temp.df$check) "  <- check this one" else ""))
  }

  if(i < nrow(locations)){
    Sys.sleep(3)
  }
}

if (length(not_found) > 0) {
  message("\nNot found (add the country to country_codes, or fix the city by hand):\n  ",
          paste(not_found, collapse = "\n  "))
}

coord <- coord %>%
  select(city, state, latitude, longitude, time_zone, tz_offset) %>%
  distinct() %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

# Update saved venues in the redo locations
if (nrow(redo) > 0 && nrow(coord) > 0) {
  venues <- venues %>%
    left_join(coord %>%
                semi_join(redo, by = c("city" = "venue_address_city", "state" = "venue_address_state")) %>%
                rename_with(~ paste0("new_", .x), latitude:tz_offset),
              by = c("venue_address_city" = "city", "venue_address_state" = "state")) %>%
    mutate(latitude  = coalesce(new_latitude, latitude),
           longitude = coalesce(new_longitude, longitude),
           time_zone = coalesce(new_time_zone, time_zone),
           tz_offset = coalesce(new_tz_offset, tz_offset)) %>%
    select(-starts_with("new_"))
}


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

# Home team for each new venue: non-neutral home games on the schedule, played or
# not (so a team's arena is a team venue from the start of the season)
missing_venues <- missing_venues %>%
  left_join(schedule %>%
              filter(neutral_site == FALSE, venue_id %in% missing_venues$venue_id) %>%
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

# D1 teams with no home venue (e.g. new D1 teams whose arena was saved as a neutral
# site before they had played a game): use the venue of most of their non-neutral
# home games and move it from the neutral sites to the team venues
home_fix <- schedule %>%
  filter(neutral_site == FALSE, !is.na(venue_id)) %>%
  mutate(team_id = as.character(home_id)) %>%
  filter(team_id %in% d1_teams, team_id %!in% team_venues$team_id) %>%
  count(team_id, venue_id) %>%
  group_by(team_id) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(team_id, venue_id) %>%
  inner_join(bind_rows(team_venues, neutral_sites) %>%
               select(-team_id) %>%
               distinct(venue_id, .keep_all = TRUE), by = "venue_id")

if (nrow(home_fix) > 0) {
  cat("\nHome venues added for teams that had none:\n")
  print(home_fix %>% select(team_id, venue_full_name, venue_address_city, venue_address_state))
  team_venues <- bind_rows(team_venues, home_fix)
  neutral_sites <- neutral_sites %>% filter(venue_id %!in% home_fix$venue_id)
}

no_home <- setdiff(d1_teams, team_venues$team_id)
if (length(no_home) > 0) message("\nD1 teams still without a home venue: ", paste(no_home, collapse = ", "))


saveRDS(team_venues, "team_venues_wbb.rds")
saveRDS(neutral_sites, "neutral_sites_wbb.rds")

# Venues on the schedule still missing (should be empty, or only the not-found ones)
schedule %>%
  filter(!is.na(venue_id),
         venue_id %!in% team_venues$venue_id &
           venue_id %!in% neutral_sites$venue_id) %>%
  select(venue_address_city, venue_address_state) %>%
  distinct()
