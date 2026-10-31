# ──────────────────────────────────────────────────────────────────────────────
# 0. Archive Ratings Pages.R  (both leagues)
#
# Run right after you set dante_season in _Helper Files/season.R (step 0).
# Copies the current ratings pages, Pages/vrgl_ratings.qmd and Pages/btrc_ratings.qmd,
# to Pages/vrgl_ratings_<finished season>.qmd and Pages/btrc_ratings_<finished season>.qmd,
# fixed to that season. The archive page then shows the final ratings, champions and
# seeds, and every page's "Other Seasons" line picks it up by itself.
#
# The current pages follow dante_season, so they move to the new season on their
# own: they say "posted once the preseason ratings are built" until step 6 makes the
# in-season starter file, then show the priors until the daily run takes over.
#
# Won't overwrite a page that already exists.
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
source(dante_path("_Helper Files", "season.R"))
C <- dante_completed

for (lg in c("vrgl", "btrc")) {
  lab  <- c(vrgl = "MBB", btrc = "WBB")[[lg]]
  src  <- dante_path("Pages", sprintf("%s_ratings.qmd", lg))
  dest <- dante_path("Pages", sprintf("%s_ratings_%d.qmd", lg, C))
  if (file.exists(dest)) {
    message(basename(dest), " already exists; left it alone. (If ", C, " isn't the season that just ",
            "finished, set dante_season in _Helper Files/season.R first.)")
    next
  }
  txt <- readLines(src, warn = FALSE)
  i_season <- grep("^season <- dante_season", txt)
  i_title  <- grep(sprintf('^title: "%s Ratings"$', lab), txt)
  if (length(i_season) != 1 || length(i_title) != 1)
    stop(basename(src), ": expected one 'season <- dante_season' line and one title line ",
         sprintf('\'title: "%s Ratings"\'', lab), "; found ", length(i_season), " and ", length(i_title))
  txt[i_season] <- sprintf("season <- %d", C)
  txt[i_title]  <- sprintf('title: "%s Ratings - %d"', lab, C)
  writeLines(txt, dest)
  cat("Wrote", dest, "\n")
}
cat("Commit and push the new pages; the site picks them up on the next run.\n")
