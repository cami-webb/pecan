#!/usr/bin/env Rscript

library(PEcAn.data.land)
# devtools::load_all("modules/data.land")
devtools::load_all("models/sipnet")

source("workflows/sipnet-restart-workflow/81-utils.R")

config <- config::get(file = "workflows/sipnet-restart-workflow/config.yml")

outdir_root <- config[["outdir_root"]]
# Cleanup some existing files to have a clean start
list.files(outdir_root, full.names = TRUE) |>
  grepv(pattern = "events.json", invert = TRUE, fixed = TRUE) |>
  unlink(recursive = TRUE)

binary <- config[["sipnet_binary"]]
stopifnot(file.exists(binary))

n_ensemble <- config[["n_ensemble"]]

site_id <- config[["site_id"]]
dp_data <- read.csv(config[["dp_path"]]) |>
  dplyr::filter(.data$id == .env$site_id)
site_lat <- dp_data[["lat"]]
site_lon <- dp_data[["lon"]]

events_json_file <- file.path(outdir_root, "events.json")
sipnet_eventfile <- PEcAn.SIPNET::write.events.SIPNET(events_json_file, outdir_root)
events <- jsonlite::read_json(events_json_file, simplifyVector = FALSE)

all_dates <- events |>
  purrr::pluck(1, "events") |>
  purrr::map_chr("date") |>
  as.Date()

start_date <- min(all_dates)
end_date <- max(all_dates)


met <- file.path(config[["met_dir"]], site_id) |>
  list.files("ERA5\\..*\\.clim", full.names = TRUE) |>
  as.list()
names(met) <- paste0("path", seq_along(met))

icfile <- file.path(config[["ic_dir"]], site_id) |>
  list.files("IC_site_.*\\.nc", full.names = TRUE) |>
  as.list()
names(icfile) <- paste0("path", seq_along(icfile))

pft_dir <- config[["pft_dir"]]
stopifnot(dir.exists(pft_dir))

################################################################################
outdir <- fs::path_abs(file.path(outdir_root, "output"))

pfts <- c("temperate.deciduous", "grass", "annual_crop") |>
  purrr::map(~list(
    name = .x,
    posterior.files = file.path(pft_dir, .x, "post.distns.Rdata"),
    outdir = paste0(file.path(pft_dir, .x), "/")
  )) |>
  c(list(list(name = "soil", outdir = file.path(pft_dir, "soil/"))))
names(pfts) <- rep("pft", length(pfts))

settings_outdir <- file.path(outdir, "out")
dir.create(settings_outdir, showWarnings = FALSE, recursive = TRUE)
settings_rundir <- file.path(outdir, "run")
dir.create(settings_rundir, showWarnings = FALSE, recursive = TRUE)

ensemble_settings <- list(
  size = n_ensemble,
  variable = "LAI",
  variable = "SoilMoist",
  variable = "GPP",
  variable = "SoilResp",
  variable = "AGB",
  variable = "NEE",
  samplingspace = list(
    parameters = list(method = "uniform"),
    events = list(method = "sampling"),
    met = list(method = "sampling"),
    poolinitcond = list(method = "sampling"),
    # leaf_phenology = list(method = "sampling")
    NULL
  ),
  start.year = lubridate::year(start_date),
  end.year = lubridate::year(end_date)
)

settings_raw <- PEcAn.settings::as.Settings(list(
  outdir = outdir_root,
  modeloutdir = settings_outdir,
  rundir = settings_rundir,
  pfts = pfts,
  ensemble = ensemble_settings,
  model = list(
    type = "SIPNET",
    binary = binary,
    revision = "v2",
    options = list(
      GDD = 0,
      NITROGEN_CYCLE = 0,
      ANAEROBIC = 0,
      LITTER_POOL = 1
    )
  ),
  run = list(
    site = list(
      id = site_id,
      name = site_id,
      lat = site_lat,
      lon = site_lon,
      site.pft = list(
        soil = "soil"
      )
    ),
    start.date = start_date,
    end.date = end_date,
    inputs = list(
      met = list(path = met),
      poolinitcond = list(path = icfile),
      events = list(path = sipnet_eventfile)
    )
  ),
  host = list(
    name = "localhost"
  )
))
# Get parameter samples for all relevant PFTs
sens_design <- PEcAn.uncertainty::generate_joint_ensemble_design(
  settings_raw,
  settings_raw$ensemble$size
)

settings <- PEcAn.workflow::runModule.run.write.configs(
  settings_raw,
  input_design = sens_design$X
)

inputs_runs <- file.path(settings$outdir, "runs_manifest.csv") |>
  read.csv() |>
  cbind(sens_design[["X"]])

write.csv(inputs_runs, file = file.path(settings$outdir, "inputs_runs.csv"))

################################################################################
# Begin loop
for (irun in seq_len(nrow(inputs_runs))) {
  # irun <- 1
  run_row <- inputs_runs[irun, ]
  run_id <- run_row[["run_id"]]
  run_dir <- file.path(settings$rundir, run_id)
  run_outdir <- file.path(settings$modeloutdir, run_id)
  run_settings <- subset_paths(settings, run_row)

  ens_samples_file <- file.path(
    run_settings$outdir,
    sprintf("ensemble.samples.%s.Rdata", run_settings$ensemble$ensemble.id)
  )
  stopifnot(file.exists(ens_samples_file))
  ensemble_samples <- PEcAn.utils::load_local(ens_samples_file)[["ens.samples"]]
  run_traits <- lapply(ensemble_samples, \(dat) dat[run_row[["param"]], ])

  # TODO: Different runs might have different events.json files. But for now,
  # this is hard coded to a single set of events.
  run_events_json <- events_json_file
  crop_cycles <- events_to_crop_cycle_starts(run_events_json)

  # Get segments
  segments <- data.frame(
    start_date = c(run_settings$run$start.date, crop_cycles[["date"]]),
    end_date = c(crop_cycles[["date"]] - 1, run_settings$run$end.date),
    crop_code = c(NA_character_, crop_cycles[["crop_code"]])
  )
  segments[["pft"]] <- crop2pft(segments[["crop_code"]])
  segments[["segment_id"]] <- sprintf("%03d", seq_len(nrow(segments)))
  segments[["segment_dir"]] <- file.path(
    run_dir,
    "segments",
    sprintf("segment_%s", segments[["segment_id"]])
  )

  segment_root <- file.path()

  for (isegment in seq_len(nrow(segments))) {
    # isegment <- 1
    segment <- segments[isegment, ]
    segment_id <- segment[["segment_id"]]
    dstart <- segment[["start_date"]]
    dend <- segment[["end_date"]]
    segment_dir <- segment[["segment_dir"]]

    runid_dummy <- "1"

    unlink(segment_dir, recursive = TRUE)
    dir.create(segment_dir, showWarnings = FALSE, recursive = TRUE)

    segment_inputs <- split_inputs.SIPNET(
      dstart,
      dend,
      run_settings$run$inputs,
      overwrite = TRUE,
      outpath = segment_dir
    )

    # Segment-specific settings
    segment_outdir <- file.path(segment_dir, "out")
    dir.create(segment_outdir, showWarnings = FALSE, recursive = TRUE)
    segment_rundir <- file.path(segment_dir, "run")
    dir.create(segment_rundir, showWarnings = FALSE, recursive = TRUE)
    file.create(file.path(segment_rundir, "README.txt"))

    segment_rundir_withid <- file.path(segment_rundir, runid_dummy)
    dir.create(segment_rundir_withid, showWarnings = FALSE, recursive = TRUE)
    segment_outdir_withid <- file.path(segment_outdir, runid_dummy)
    dir.create(segment_outdir_withid, showWarnings = FALSE, recursive = TRUE)

    segment_settings <- run_settings
    segment_settings[["outdir"]] <- segment_outdir
    segment_settings[["modeloutdir"]] <- segment_outdir
    segment_settings[["rundir"]] <- segment_rundir
    segment_settings[[c("run", "start.date")]] <- dstart
    segment_settings[[c("run", "end.date")]] <- dend
    segment_settings[[c("run", "inputs")]] <- segment_inputs
    segment_settings
    if (is.null(segment_settings[[c("model", "options")]])) {
      segment_settings[[c("model", "options")]] <- list()
    }

    if (isegment > 1) {
      # For isegment > 1, we restart from the *previous* segment's restart.out
      segment_settings[[c("model", "options", "RESTART_IN")]] <- restart_out
    }
    # ...and now, define a new restart.out for *this* segment
    restart_out <- file.path(segment_rundir, "restart.out")
    segment_settings[[c("model", "options", "RESTART_OUT")]] <- restart_out

    segment_traits <- run_traits[[segment[["pft"]]]]

    # Write dummy runs file
    writeLines(runid_dummy, file.path(segment_rundir, "runs.txt"))

    config <- PEcAn.SIPNET::write.config.SIPNET(
      defaults = segment_settings[["pfts"]],
      trait.values = segment_traits,
      settings = segment_settings,
      run.id = runid_dummy
    )

    runs <- PEcAn.workflow::start_model_runs(segment_settings, write = FALSE)

    model2netcdf.SIPNET(
      outdir = segment_outdir_withid,
      sitelat = segment_settings[[c("run", "site", "lat")]],
      sitelon = segment_settings[[c("run", "site", "lon")]],
      start_date = dstart,
      end_date = dend,
      revision = segment_settings[[c("model", "revision")]],
      overwrite = TRUE
    )
  }

  segment_ncfiles <- lapply(
    segments[["segment_dir"]],
    \(x) {
      list.files(
        file.path(x, "out", "1"),
        pattern = "\\d+\\.nc",
        full.names = TRUE
      )
    }
  ) |>
    do.call(what = c)

  segment_files_byyear <- split(
    segment_ncfiles,
    factor(basename(segment_ncfiles))
  )
  segment_outfiles <- file.path(run_outdir, names(segment_files_byyear))
  results <- purrr::map2(
    segment_files_byyear,
    segment_outfiles,
    PEcAn.SIPNET::mergeNC
  )
}
