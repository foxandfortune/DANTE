# =============================================================================
# Update schedules and team box using hoopR replacement functions
#
# Output schema matches the 86-column hoopR schedule exactly, including:
#   - Bind to prior results
#   - Save for updated runs
# =============================================================================


`%!in%` = Negate(`%in%`)


source("load_mbb_schedule_espn.R")
mbb_schedule_base <- hoopR::load_mbb_schedule(seasons = 2026) %>% 
  filter(game_date <= as.Date("2026-03-06"))
schedule_new      <- load_mbb_schedule_espn(start_date = "2026-03-07")


schedule_new <- schedule_new %>% 
  mutate(venue_id = as.integer(venue_id),
         uid = as.integer(uid))

schedule_new <- schedule_new %>% 
  mutate(uid = as.integer(uid))

glimpse(mbb_schedule_base)
glimpse(schedule_new)

mbb_schedule_full <- bind_rows(mbb_schedule_base, schedule_new)


saveRDS(mbb_schedule_full, 'updated_schedule_2026.rds')

source("load_mbb_team_box_espn.R")

mbb_team_box_base <- hoopR::load_mbb_team_box(seasons = 2026) %>% 
  filter(game_date <= as.Date("2026-03-06"))

team_box_new <- load_mbb_team_box_espn(
  schedule   = schedule_new,
  start_date = "2026-03-07",
  end_date   = as.character(Sys.Date() - 1)
)


mbb_team_box_full <- bind_rows(mbb_team_box_base, team_box_new)
saveRDS(mbb_team_box_full, 'updated_team_box_2026.rds')

team_box_new
glimpse(team_box_new)
diag_team_box(401804963)




source("load_mbb_pbp_espn.R")

mbb_pbp_base <- hoopR::load_mbb_pbp(seasons = 2026) %>%
  filter(game_date <= as.Date("2026-03-06"))

pbp_new <- load_mbb_pbp_espn(
  schedule   = mbb_schedule_full,
  start_date = "2026-03-07"
)

mbb_pbp_full <- bind_rows(mbb_pbp_base, pbp_new)
saveRDS(mbb_pbp_full, 'updated_pbp_2026.rds')