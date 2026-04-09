#!/usr/bin/env Rscript

if (FALSE) {
  devtools::install("models/sipnet", upgrade = FALSE)
  devtools::install("modules/data.land", upgrade = FALSE)
}

config <- config::get(file = "workflows/sipnet-restart-workflow/config.yml")

do_run_sipnet_segmented <- function(irun) {
  source("workflows/sipnet-restart-workflow/utils.R", local = TRUE)
  config <- config::get(file = "workflows/sipnet-restart-workflow/config.yml")
  settings <- PEcAn.settings::read.settings(file.path(config[["outdir_root"]], "settings.xml"))
  inputs_runs <- read.csv(file.path(settings$outdir, "inputs_runs.csv"))
  run_sipnet_segmented(settings, inputs_runs[irun, ])  # nolint
}

# Run in parallel using crew
nworkers <- pmin(config[["n_ensemble"]], parallel::detectCores())
controller <- crew::crew_controller_local(workers = nworkers)
crew_results <- controller$map(
  command = do_run_sipnet_segmented(irun),
  iterate = list(irun = seq_len(config[["n_ensemble"]])),
  data = list(settings = settings, do_run_sipnet_segmented = do_run_sipnet_segmented)
)

# Or, to run sequentially:
# for (i in seq_len(config[["n_ensemble"]])) do_run_sipnet_segmented(i)   # nolint
