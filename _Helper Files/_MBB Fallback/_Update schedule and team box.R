# =============================================================================
# Update schedules and team box using hoopR replacement functions
#
# Output schema matches the 86-column hoopR schedule exactly, including:
#   - Bind to prior results
#   - Save for updated runs
# =============================================================================

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

`%!in%` = Negate(`%in%`)

# ==========================================================
# Update schedule -------
# ==========================================================
source("load_mbb_schedule_espn.R")

# Load schedule prior to March 7
mbb_schedule_base <- hoopR::load_mbb_schedule(seasons = 2026) %>% 
  filter(game_date <= as.Date("2026-03-06"))

# Load new schedule
schedule_new      <- load_mbb_schedule_espn(start_date = "2026-03-07",
                                            end_date = "2026-04-10")

schedule_new <- schedule_new %>% 
  filter(game_id %!in% mbb_schedule_base$game_id)

# Add to old schedule
mbb_schedule_full <- bind_rows(mbb_schedule_base, schedule_new)

# Save
saveRDS(mbb_schedule_full, 'updated_schedule_2026.rds')


# ==========================================================
# Update Team Box -------
# ==========================================================
source("load_mbb_team_box_espn.R")

# Load team box prior to March 7
mbb_team_box_base <- hoopR::load_mbb_team_box(seasons = 2026) %>% 
  filter(game_date <= as.Date("2026-03-06"))

# Load new team box data
team_box_new <- load_mbb_team_box_espn(
  schedule   = schedule_new,
  start_date = "2026-03-07",
  end_date   = as.character(Sys.Date() - 1)
)

# Add to priors --------------------
mbb_team_box_full <- bind_rows(mbb_team_box_base, team_box_new)

# Save 
saveRDS(mbb_team_box_full, 'updated_team_box_2026.rds')

# ==========================================================
# Update PBP -------
# ==========================================================
source("load_mbb_pbp_espn.R")

# Pull missing pbp (Adjust based on what's been added)
pbp_new <- load_mbb_pbp_espn(
  schedule   = schedule_new,
  start_date = "2026-03-07"
)

# Save ------------------
saveRDS(pbp_new, 'updated_pbp_2026.rds')

# ==========================================================
# Update Player Box -------
# ==========================================================
source("load_mbb_player_box_espn.R")

# Load data prior to March 7
player_box_base <- hoopR::load_mbb_player_box(seasons = 2026) %>%
  filter(game_date <= as.Date("2026-03-06")) 

# Pull new data
player_box_new <- load_mbb_player_box_espn(
  schedule   = schedule_new,
  start_date = "2026-03-07"
)

# Add to base data 
mbb_player_box_full <- bind_rows(player_box_base,
                                 player_box_new) 

# Save
saveRDS(mbb_player_box_full, 'updated_player_box_2026.rds')
