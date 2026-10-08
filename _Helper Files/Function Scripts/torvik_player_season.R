# Drop-in replacement for cbbdata::cbd_torvik_player_season()
#
# cbbdata stopped updating Torvik data after the 2024-25 season, so year = 2026
# comes back empty. This pulls the same player-season table straight from
# barttorvik.com (getadvstats.php CSV, no header row) and returns it with the
# cbbdata column names the DANTE recruiting/prior scripts use:
#   player, team, conf, exp, pos, g, mpg, ppg, oreb, dreb, apg, tov, efg,
#   obpm, dbpm, oreb_rate, dreb_rate, ast, to, stl, year (+ extra Torvik columns)
#
# Column mapping was checked against cbbdata's 2025 values (Ace Bailey, Egor
# Demin, Robby Carmody). Notes:
#   - obpm / dbpm are Torvik's "gbpm" splits (cols 56-57), as in cbbdata.
#     Torvik's other BPM split is kept as torvik_obpm / torvik_dbpm.
#   - tov (per game) isn't in the CSV; cbbdata derived it as apg / ast_to,
#     with 0 when ast_to is 0. Same here.
#   - ast, to, stl, oreb_rate, dreb_rate are rates (%), as in cbbdata.
#
# Usage:
#   source('_Helper Files/Function Scripts/torvik_player_season.R')
#   torvik_player_season(2026)
#   torvik_player_season(2026, file = "getadvstats_2026.csv")  # if the site blocks R,
#       # open https://barttorvik.com/getadvstats.php?year=2026&csv=1 in a browser,
#       # save the file, and read it from disk instead

torvik_player_season <- function(year, file = NULL) {
  torvik_cols <- c(
    "player", "team", "conf", "g", "min_pct", "ortg", "usg", "efg", "ts",
    "oreb_rate", "dreb_rate", "ast", "to", "ftm", "fta", "ft_pct",
    "two_m", "two_a", "two_pct", "three_m", "three_a", "three_pct",
    "blk", "stl", "ftr", "exp", "hgt", "num", "porpag", "adjoe", "pfr",
    "year", "id", "hometown", "rec_rank", "ast_to",
    "rim_m", "rim_a", "mid_m", "mid_a", "rim_pct", "mid_pct",
    "dunk_m", "dunk_a", "dunk_pct", "pick",
    "drtg", "adrtg", "dporpag", "stops",
    "torvik_bpm", "torvik_obpm", "torvik_dbpm", "bpm", "mpg", "obpm", "dbpm",
    "oreb", "dreb", "rpg", "apg", "spg", "bpg", "ppg",
    "pos", "three_a_100", "dob"
  )

  chr_cols <- c("player", "team", "conf", "exp", "hgt", "num", "hometown", "pos", "dob")
  col_spec <- readr::cols(.default = readr::col_double())
  col_spec$cols[chr_cols] <- list(readr::col_character())

  if (is.null(file)) {
    url  <- glue::glue("https://barttorvik.com/getadvstats.php?year={year}&csv=1")
    resp <- httr::GET(url, httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    body <- httr::content(resp, as = "text", encoding = "UTF-8")

    if (httr::http_error(resp) || grepl("^\\s*<", body)) {
      stop(glue::glue(
        "barttorvik.com didn't return CSV for {year} (HTTP {httr::status_code(resp)}). ",
        "Download {url} in a browser and pass it with file = ."
      ))
    }
    src <- I(body)
  } else {
    src <- file
  }

  stats <- readr::read_csv(src, col_names = torvik_cols, col_types = col_spec,
                           progress = FALSE, show_col_types = FALSE)

  if (ncol(stats) != length(torvik_cols) || nrow(readr::problems(stats)) > 0) {
    warning("Torvik CSV layout may have changed - check readr::problems() and the column mapping.")
  }

  stats %>%
    dplyr::filter(.data$year == !!year) %>%
    dplyr::mutate(tov = dplyr::if_else(!is.na(ast_to) & ast_to > 0, apg / ast_to, 0)) %>%
    dplyr::relocate(player, team, conf, exp, pos, g, mpg, ppg, oreb, dreb, apg, tov,
                    efg, obpm, dbpm, oreb_rate, dreb_rate, ast, to, stl, year) %>%
    as.data.frame()
}
