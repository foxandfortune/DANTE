library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# ──────────────────────────────────────────────────────────────────────────────
# 4. Create Priors - MBB.R
#
# Preseason prior for every rating (pace, ast, oreb, to, rtg, efg, raw_rating),
# offense and defense, from:
#   - the last two seasons' full-season ratings,
#   - conference-mates' ratings,
#   - departures (draft entrants, transfers out, seniors) and incoming transfers,
#     weighted by minutes and BPM,
#   - 247 recruits.
# Which of these each stat uses was chosen by the backtest
# (_Backtest/priors/_Test Priors - All Stats.R) and is saved in
# prior_models_mbb.rds. Every model is a plain linear regression refit here on all
# past seasons.
#
# Teams with no 247 page: commits and incoming transfers are unknown, so they are
# 0 and the no_247 flag carries the offset. Teams new to D1: no last-season rating,
# so it is 0 and the new_d1 flag carries the offset (the model learns how far
# below average new teams start).
#
# Needs, for prior_season S:
#   - full-season ratings through S-1 in "Full Season - Current Weights"
#     (Full-Season Ratings - All.R writes there);
#   - rosters/roster_{S-1}.rds;
#   - recruiting files *_clean_{S-1}.rds (scripts 1-3);
#   - Conferences/mbb_conf_{S}.rds (_Get CBB Conferences.R). Without it, last
#     season's teams are used and teams new to D1 get no prior.
# No cbbdata login needed: player stats come from the recruiting files (Torvik)
# and the ESPN box scores.
# ──────────────────────────────────────────────────────────────────────────────

# Set working directory
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
setwd(dante_path())

# Set season for priors
source(dante_path("_Helper Files", "season.R"))
prior_season <- dante_season

source('_Helper Files/Function Scripts/prior_functions.R')

# Check inputs ---------
cruit_dir <- 'VRGL/Stats/Offseason Updates - MBB/Recruiting and Priors/Recruiting - MBB'
needed <- c(file.path(cruit_dir, sprintf('%s_clean_%d.rds',
                                         c('commits', 'transfers', 'draft_entrants', 'graduates'),
                                         prior_season - 1)),
            glue::glue('VRGL/Stats/Team and Player Stats/Rosters/roster_{prior_season - 1}.rds'))
if (any(!file.exists(needed))) stop("Missing inputs:\n", paste(needed[!file.exists(needed)], collapse = "\n"))

models <- readRDS('VRGL/Stats/Team and Player Stats/Power Ratings/Weights/prior_models_mbb.rds')
fs_dir <- 'VRGL/Stats/Team and Player Stats/Power Ratings/Team Ratings/Full Season - Current Weights'

# Build ---------
priors.all <- build_priors("MBB", prior_season, models, fs_dir = fs_dir)

# Checks ---------
teams <- readRDS('_Helper Files/Team Data/team_database.rds')
stopifnot(all(c("pace", "ast", "oreb", "to", "rtg", "efg", "raw_rating") %in% names(priors.all)),
          !any(map_lgl(priors.all, ~ any(is.na(.x$value)))))

top <- priors.all$raw_rating %>%
  filter(str_detect(name, "_id_")) %>%
  mutate(side = if_else(str_starts(name, "team_id_"), "off", "def"),
         team_id = str_extract(name, "\\d+$")) %>%
  select(team_id, side, value) %>%
  pivot_wider(names_from = side, values_from = value) %>%
  mutate(net = off - def) %>%
  left_join(teams %>% transmute(team_id = as.character(team_id), team), by = "team_id") %>%
  arrange(desc(net))
print(head(top, 25), n = 25)

cat("\nTeams new to D1:", paste(teams$team[as.character(teams$team_id) %in% attr(priors.all, "new_d1")], collapse = ", "), "\n")
cat("Teams in the prior:", nrow(top), "\n")

## Save priors --------
saveRDS(priors.all,
        glue::glue("VRGL/Stats/Team and Player Stats/Power Ratings/Team Ratings/Priors/priors_{prior_season}.rds"))

### As starter for all ratings --------
saveRDS(priors.all,
        glue::glue("VRGL/Stats/Team and Player Stats/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_{prior_season}.rds"))
