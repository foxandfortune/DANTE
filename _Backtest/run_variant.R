# Usage: Rscript run_variant.R <variant> <league>
bt_dir <- if (dir.exists("_Backtest")) "_Backtest" else "/home/claude/_Backtest"
for (f in list.files(file.path(bt_dir, "R"), full.names = TRUE)) source(f)
args <- commandArgs(TRUE); v <- args[1]; lg <- args[2]
source(file.path(bt_dir, "variants.R"))
t0 <- Sys.time()
p <- run_model(variants[[v]], lg, bt_seasons[[lg]], priors = "prod", cores = 2)
dir.create(file.path(bt_dir, "output"), showWarnings = FALSE)
saveRDS(p, file.path(bt_dir, "output", glue::glue("pred_{v}_{lg}.rds")))
cat(v, lg, "done in", round(as.numeric(Sys.time()-t0, units="mins"),1), "min, rows", nrow(p), "\n")
