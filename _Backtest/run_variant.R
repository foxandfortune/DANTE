# Usage: Rscript run_variant.R <variant> <league>
bt_dir <- if (dir.exists("_Backtest")) "_Backtest" else "/home/claude/_Backtest"
for (f in list.files(file.path(bt_dir, "R"), full.names = TRUE)) source(f)
args <- commandArgs(TRUE); v <- args[1]; lg <- args[2]
source(file.path(bt_dir, "variants.R"))
t0 <- Sys.time()
seas <- Sys.getenv("BT_SEASONS", "")   # e.g. "2021:2026" to run only the seasons with test priors
seas <- if (nzchar(seas)) eval(parse(text = seas)) else bt_seasons[[lg]]
p <- run_model(variants[[v]], lg, seas, priors = "prod", cores = 2)
dir.create(file.path(bt_dir, "output"), showWarnings = FALSE)
tag <- Sys.getenv("BT_TAG", "")   # e.g. which priors folder was used
saveRDS(p, file.path(bt_dir, "output", glue::glue("pred_{v}{tag}_{lg}.rds")))
cat(v, lg, "done in", round(as.numeric(Sys.time()-t0, units="mins"),1), "min, rows", nrow(p), "\n")
