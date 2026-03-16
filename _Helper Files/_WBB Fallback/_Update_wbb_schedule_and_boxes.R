# =============================================================================
# _Update_wbb_schedule_and_boxes.R
#
# WBB equivalent of _Update_schedule_and_team_box.R
# Uses wehoop instead of hoopR, womens-college-basketball ESPN endpoints.
#
# Output schema matches wehoop::load_wbb_* exactly.
# =============================================================================

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

`%!in%` = Negate(`%in%`)

# ==========================================================
# Update Schedule -------
# ==========================================================
source("load_wbb_schedule_espn.R")

# Load schedule prior to March 7
wbb_schedule_base <- wehoop::load_wbb_schedule(seasons = 2026) %>%
  filter(game_date <= as.Date("2026-03-06"))

# Load new schedule
schedule_new <- load_wbb_schedule_espn(
  start_date = "2026-03-07",
  end_date   = "2026-04-15"
)

schedule_new <- schedule_new %>%
  filter(game_id %!in% wbb_schedule_base$game_id)

# Add to old schedule
wbb_schedule_full <- bind_rows(wbb_schedule_base, schedule_new)

# Save
saveRDS(wbb_schedule_full, 'updated_wbb_schedule_2026.rds')


# ==========================================================
# Update Team Box -------
# ==========================================================
source("load_wbb_team_box_espn.R")

# Load team box prior to March 7
wbb_team_box_base <- wehoop::load_wbb_team_box(seasons = 2026) %>%
  filter(game_date <= as.Date("2026-03-06"))

# Load new team box data
team_box_new <- load_wbb_team_box_espn(
  schedule   = schedule_new,
  start_date = "2026-03-07",
  end_date   = as.character(Sys.Date() - 1)
)

# Add to priors
wbb_team_box_full <- bind_rows(wbb_team_box_base, team_box_new)

# Save
saveRDS(wbb_team_box_full, 'updated_wbb_team_box_2026.rds')


# ==========================================================
# Update PBP -------
# ==========================================================
source("load_wbb_pbp_espn.R")

# Pull missing PBP
pbp_new <- load_wbb_pbp_espn(
  schedule   = schedule_new,
  start_date = "2026-03-07"
)

# Save
saveRDS(pbp_new, 'updated_wbb_pbp_2026.rds')


# ==========================================================
# Update Player Box -------
# ==========================================================
source("load_wbb_player_box_espn.R")

# Load data prior to March 7
wbb_player_box_base <- wehoop::load_wbb_player_box(seasons = 2026) %>%
  filter(game_date <= as.Date("2026-03-06"))

# Pull new data
player_box_new <- load_wbb_player_box_espn(
  schedule   = schedule_new,
  start_date = "2026-03-07"
)

# Add to base data
wbb_player_box_full <- bind_rows(wbb_player_box_base, player_box_new)

# Save
saveRDS(wbb_player_box_full, 'updated_wbb_player_box_2026.rds')
