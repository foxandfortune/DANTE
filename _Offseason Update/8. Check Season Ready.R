# ──────────────────────────────────────────────────────────────────────────────
# 8. Check Season Ready.R  (both leagues)
#
# Checks that everything the daily pipeline and sims read for the coming season
# (dante_season in _Helper Files/season.R) exists and looks sane. Prints PASS /
# WARN / FAIL per item and stops with an error if anything FAILs.
# Run it last in the offseason, and again any time before the first daily run.
# ──────────────────────────────────────────────────────────────────────────────
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
source(dante_path("_Helper Files", "season.R"))
suppressMessages({library(dplyr); library(stringr)})
S <- dante_season; C <- dante_completed

out <- list()
chk <- function(level, item, msg = "") {
  out[[length(out) + 1]] <<- data.frame(level = level, item = item, msg = msg)
  cat(sprintf("%-4s  %s%s\n", level, item, if (nzchar(msg)) paste0("  (", msg, ")") else ""))
}
must <- function(ok, item, msg = "") chk(if (isTRUE(ok)) "PASS" else "FAIL", item, if (isTRUE(ok)) "" else msg)
exists_chk <- function(path, item = basename(path)) must(file.exists(path), item, paste("missing:", path))

L <- list(
  MBB = list(base = "VRGL/Stats/Team and Player Stats", off = "VRGL/Stats/Offseason Updates - MBB",
             tag = "", conf = "mbb", teams = "_Helper Files/Team Data/team_database.rds",
             shot = "VRGL/Stats/Offseason Updates - MBB/Shooting Models/Models",
             shot_names = c(loc = "xfg {S}.json", noloc = "xfg noloc {S}.json", ft = "ft {S}.rds"),
             results = "VRGL/Stats/Season Schedules/Results"),
  WBB = list(base = "BTRC/Stats/Team and Player Stats - WBB", off = "BTRC/Stats/Offseason Updates - WBB",
             tag = "wbb_", conf = "wbb", teams = "_Helper Files/Team Data/team_database_wbb.rds",
             shot = "BTRC/Stats/Offseason Updates - WBB/Shooting Models/Models",
             shot_names = c(loc = "xfg wbb {S}.json", noloc = "xfg noloc wbb {S}.json", ft = "ft wbb {S}.rds"),
             results = "BTRC/Stats/Season Schedules/Results"))

# Features script 1 builds for the shot models (same order)
feat_loc <- c("season", "shot_dist", "shot_angle", "x", "y", "is_atrim", "is_putback", "is_home",
              "secs_remaining", "shot_diff", "shot_diff_time", "shot_clock", "is_fastbreak", "year_index",
              "pos_C", "pos_F", "pos_G", "pos_PF", "pos_PG", "pos_SF", "pos_SG", "season_type_3")
feat_noloc <- c("season", "is_atrim", "is_putback", "is_three", "is_home", "secs_remaining", "shot_diff",
                "shot_diff_time", "shot_clock", "is_fastbreak", "year_index",
                "pos_C", "pos_F", "pos_G", "pos_PF", "pos_PG", "pos_SF", "pos_SG", "season_type_3")
stats_needed <- c("pace", "ast", "oreb", "to", "rtg", "efg", "raw_rating")
wstats <- c("poss_per_40", "ast_rt", "oreb_rt", "to_rt", "rtg", "efg_adj")

cat("Season being prepared:", S, " (last finished season:", C, ")\n")

for (lg in names(L)) {
  p <- L[[lg]]; pr <- file.path(p$base, "Power Ratings")
  cat("\n=====", lg, "=====\n")

  ## Teams and conferences
  cf <- sprintf("_Helper Files/Team Data/Conferences/%s_conf_%d.rds", p$conf, S)
  exists_chk(cf)
  teams_S <- NULL
  if (file.exists(cf)) {
    conf <- readRDS(cf); db <- readRDS(p$teams); teams_S <- unique(as.character(conf$team_id))
    must(length(teams_S) >= 300, "conference file has a full D1 list", paste(length(teams_S), "teams"))
    miss <- setdiff(teams_S, as.character(db$team_id))
    must(length(miss) == 0, "every D1 team is in the team database", paste("not in team database:", paste(miss, collapse = ", ")))
  }

  ## Last season's inputs
  exists_chk(glue::glue("{pr}/Raw Data/poss_stats_with_types_{p$tag}{C}.rds"), paste("raw game data", C))
  exists_chk(glue::glue("{p$base}/Rosters/roster_{p$tag}{C}.rds"), paste("rosters", C))
  exists_chk(glue::glue("{pr}/Team Ratings/Full Season/ratings_all_{p$tag}{C}.rds"), paste("full-season ratings", C))
  cw <- file.path(pr, "Team Ratings", "Full Season - Current Weights")
  first <- if (lg == "MBB") 2018 else 2021
  have <- file.exists(file.path(cw, sprintf("ratings_all_%s%d.rds", p$tag, first:C)))
  must(all(have), paste0("Full Season - Current Weights has ", first, "-", C),
       paste("missing:", paste((first:C)[!have], collapse = ", ")))
  if (lg == "MBB") {
    rd <- file.path(p$off, "Recruiting and Priors", "Recruiting - MBB")
    for (k in c("commits", "transfers", "draft_entrants", "graduates"))
      exists_chk(file.path(rd, sprintf("%s_clean_%d.rds", k, C)), paste(k, "clean", C))
    tf <- file.path(rd, sprintf("transfers_clean_%d.rds", C))
    if (file.exists(tf)) {
      t <- readRDS(tf)
      chk(if (all(c("percentile", "class") %in% names(t))) "PASS" else "WARN", "transfers file has percentile and class",
          if (all(c("percentile", "class") %in% names(t))) "" else "prior code rebuilds the percentile, but check 3. Clean Recruiting")
    }
  }

  ## Shot models
  for (k in names(p$shot_names)) {
    f <- file.path(p$shot, glue::glue(p$shot_names[[k]]))
    exists_chk(f, paste("shot model", basename(f)))
    if (file.exists(f) && k != "ft") {
      j <- jsonlite::fromJSON(f, simplifyVector = TRUE)
      fn <- j$learner$feature_names
      want <- if (k == "loc") feat_loc else feat_noloc
      if (length(fn)) must(identical(as.character(fn), want), paste(basename(f), "features match script 1"),
                           paste("model:", paste(fn, collapse = ",")))
      else must(as.integer(j$learner$learner_model_param$num_feature) == length(want),
                paste(basename(f), "feature count matches script 1"))
      chk("INFO", paste(basename(f), "written by xgboost", paste(j$version, collapse = ".")),
          "the GitHub workflow updates xgboost to 3.x so it can read this")
    }
  }

  ## Weights
  for (st in wstats) {
    f <- file.path(pr, "Weights", sprintf("single_season_%s%s_wgt.rds", p$tag, st))
    exists_chk(f, paste("weights", basename(f)))
    if (file.exists(f)) { w <- readRDS(f); must(all(c("season", "wgt", "lambda_adj") %in% names(w)) && nrow(w) > 0, paste(basename(f), "has season/wgt/lambda_adj rows")) }
  }
  for (st in wstats) exists_chk(file.path("VRGL/Stats/Team and Player Stats/Power Ratings/Weights", sprintf("with_priors_%s_wgt.rds", st)),
                                paste("prior blend", st, "(shared by both leagues)"))

  ## Prior models, priors, starters
  pm <- file.path(pr, "Weights", sprintf("prior_models_%s.rds", tolower(lg)))
  exists_chk(pm)
  if (file.exists(pm)) {
    tested <- attr(readRDS(pm), "tested")
    ok <- !is.null(tested) && max(tested$test_seasons) >= C
    chk(if (ok) "PASS" else "WARN", "prior models tested through the finished season",
        if (ok) "" else paste("last tested through", if (is.null(tested)) "?" else max(tested$test_seasons), "- run step 5"))
  }
  pf <- file.path(pr, "Team Ratings", "Priors", sprintf("priors_%s%d.rds", p$tag, S))
  exists_chk(pf, paste("priors", S))
  exists_chk(file.path(pr, "Team Ratings", "Inseason", sprintf("inseason_ratings_all_%s%d.rds", p$tag, S)), paste("in-season starter", S))
  if (file.exists(pf)) {
    pri <- readRDS(pf)
    must(all(stats_needed %in% names(pri)), "priors have all seven stats", paste("missing:", paste(setdiff(stats_needed, names(pri)), collapse = ",")))
    must(!any(sapply(pri, function(d) any(is.na(d$value)))), "priors have no NA values")
    pint <- pri$pace$value[pri$pace$name == "(Intercept)"]
    must(length(pint) == 1 && pint > 60 && pint < 80, "pace prior intercept is 60-80 possessions", paste("it is", round(pint, 1)))
    if (!is.null(teams_S)) {
      have_t <- sub("team_id_", "", pri$rtg$name[grepl("^team_id_", pri$rtg$name)])
      miss <- setdiff(teams_S, have_t)
      must(length(miss) == 0, "every D1 team has a prior", paste("no prior:", paste(miss, collapse = ", "), "- rerun step 6 after the conference file"))
      if (file.exists(cf)) chk(if (file.mtime(pf) >= file.mtime(cf)) "PASS" else "WARN", "priors built after the conference file",
                               if (file.mtime(pf) >= file.mtime(cf)) "" else "rerun step 6")
    }
  }

  ## Calibration
  for (np in c(FALSE, TRUE)) {
    f <- file.path(pr, "Weights", sprintf("calibration_%s%s.rds", tolower(lg), if (np) "_no_prior" else ""))
    exists_chk(f)
    if (file.exists(f)) {
      cal <- readRDS(f)
      must(all(c("hfa", "phase_table") %in% names(cal)) && nrow(cal$phase_table) == 4, paste(basename(f), "has the four month phases"))
      if (!np) chk(if (C %in% cal$fitted_on_seasons) "PASS" else "WARN", paste(basename(f), "includes", C),
                   if (C %in% cal$fitted_on_seasons) "" else paste("fitted on", paste(range(cal$fitted_on_seasons), collapse = "-"), "- run step 7"))
    }
  }

  ## Results folder for the sims
  rf <- file.path(p$results, S)
  if (!dir.exists(rf)) { dir.create(rf, recursive = TRUE); file.create(file.path(rf, ".gitkeep")) }
  must(dir.exists(rf), paste("results folder", rf))
}

## Website: last season's archive pages and the current pages
cat("\n===== Website =====\n")
for (lg in c("vrgl", "btrc")) {
  f <- file.path("Pages", sprintf("%s_ratings_%d.qmd", lg, C))
  chk(if (file.exists(f)) "PASS" else "WARN", paste("archive page", basename(f)),
      if (file.exists(f)) "" else "run _Offseason Update/0. Archive Ratings Pages.R")
  cur <- readLines(file.path("Pages", sprintf("%s_ratings.qmd", lg)), warn = FALSE)
  must(any(grepl("^season <- dante_season", cur)), paste(lg, "_ratings.qmd follows dante_season", sep = ""),
       "the current page should set season <- dante_season")
}

## Scripts still holding their own year
cat("\n===== Scripts =====\n")
daily <- c(list.files("VRGL", "\\.R$", full.names = TRUE), list.files("BTRC", "\\.R$", full.names = TRUE))
daily <- daily[!grepl("5\\. Create Ratings Table", daily)]
hard <- daily[sapply(daily, function(f) any(grepl("^(cur_yr|season) <- 20[0-9]{2}", readLines(f, warn = FALSE))))]
must(length(hard) == 0, "daily and tournament scripts read the year from season.R", paste("hard-coded year in:", paste(basename(hard), collapse = "; ")))
wf <- readLines(".github/workflows/vrgl-pipeline.yml", warn = FALSE)
on <- any(grepl("^\\s+schedule:", wf)) && any(grepl("^\\s+- cron:", wf))
chk(if (on) "PASS" else "WARN", "daily schedule enabled in vrgl-pipeline.yml",
    if (on) "" else "uncomment the schedule block once the first games are played, then commit and push")

res <- bind_rows(out)
cat("\n", sum(res$level == "PASS"), "passed,", sum(res$level == "WARN"), "warnings,", sum(res$level == "FAIL"), "failed\n")
if (any(res$level == "FAIL")) stop("Not ready: fix the FAIL items above")
