#!/usr/bin/env Rscript

devtools::load_all("modules/data.land")
devtools::load_all("models/sipnet")

config <- config::get(file = "modules/data.land/inst/sipnet-restart-workflow/config.yml")

outdir_root <- config[["outdir_root"]]

binary <- config[["sipnet_binary"]]
stopifnot(file.exists(binary))

site_id <- config[["site_id"]]
dp_data <- read.csv(config[["dp_path"]]) |>
  dplyr::filter(.data$id == .env$site_id)
site_lat <- dp_data[["lat"]]
site_lon <- dp_data[["lon"]]

events_json_file <- fs::path(outdir_root, "events.json")
events <- jsonlite::read_json(events_json_file, simplifyVector = FALSE)

all_dates <- events |>
  purrr::pluck(1, "events") |>
  purrr::map_chr("date") |>
  as.Date()

start_date <- min(all_dates)
end_date <- max(all_dates)

met <- file.path(
  config[["met_dir"]],
  site_id,
  "ERA5.1.2016-01-01.2024-12-31.clim"
)
stopifnot(file.exists(met))

icfile <- file.path(
  config[["ic_dir"]],
  site_id,
  glue::glue("IC_site_{site_id}_1.nc")
)
stopifnot(file.exists(icfile))

################################################################################
outdir <- fs::path(outdir_root, "segments")
unlink(outdir, recursive = TRUE)

settings <- PEcAn.settings::as.Settings(list(
  outdir = file.path(outdir, "out"),
  rundir = file.path(outdir, "run"),
  modeloutdir = file.path(outdir, "out"),
  pfts = list(list(
    name = "grassland",
    constants = list(num = 1)
  )),
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
      lon = site_lon
    ),
    start.date = start_date,
    end.date = end_date,
    inputs = list(
      met = list(path = met),
      poolinitcond = list(path = icfile)
    )
  ),
  host = list(
    name = "localhost"
  )
))

crop_cycles <- events_to_crop_cycle_starts(events_json_file)
# Empty example
# crop_cycles <- tibble::tibble(
#   site_id = character(0),
#   date = as.Date(NULL),
#   crop_code = character(0)
# )

# TODO: Iterate over events
site_events_obj <- events[[1]]

site_id <- site_events_obj[["site_id"]]
site_events_list <- site_events_obj[["events"]]
site_events_common <- site_events_obj
site_events_common[["events"]] <- NULL

# Get segments
segments <- tibble::tibble(
  start_date = c(start_date, crop_cycles[["date"]]),
  end_date = c(crop_cycles[["date"]] - 1, end_date)
) |>
  dplyr::mutate(
    segment_id = sprintf("%03d", dplyr::row_number()),
    segment_dir = file.path(fs::path_abs(outdir), paste0("segment_", segment_id))
  )

################################################################################

for (isegment in seq_len(nrow(segments))) {
  message("Running segment ", isegment)
  segment <- segments[isegment, ]
  segment_id <- segment[["segment_id"]]
  dstart <- segment[["start_date"]]
  dend <- segment[["end_date"]]

  segment_dir <- segment[["segment_dir"]]
  if (dir.exists(segment_dir)) {
    unlink(segment_dir, recursive = TRUE)
  }
  dir.create(segment_dir, showWarnings = FALSE, recursive = TRUE)

  # Filter events to relevant dates
  events_sub <- site_events_list |>
    purrr::keep(~as.Date(.x[["date"]]) >= dstart) |>
    purrr::keep(~as.Date(.x[["date"]]) <= dend)

  # Segment-separated events file
  eventfile <- file.path(segment_dir, "events.json")
  segment_event_obj <- list(c(site_events_common, events = list(events_sub)))
  jsonlite::write_json(segment_event_obj, eventfile, auto_unbox = TRUE, pretty = TRUE)

  segment_eventfile <- PEcAn.SIPNET::write.events.SIPNET(eventfile, segment_dir)

  metpath <- settings[[c("run", "inputs", "met", "path")]]

  # Subset the met to only the dates in this segment. SIPNET does not respect
  # start/end date, only the dates in the .clim file.
  met_segment_file <- split_inputs.SIPNET(
    dstart,
    dend,
    metpath,
    outpath = segment_dir,
    overwrite = TRUE
  )

  runid <- "1"
  # Segment-specific settings
  segment_outdir <- file.path(segment_dir, "out")
  dir.create(segment_outdir, showWarnings = FALSE, recursive = TRUE)
  segment_outdir_withid <- file.path(segment_outdir, runid)
  # Don't need to create the outdir here because it is created by write.configs
  segment_rundir <- file.path(segment_dir, "run")
  dir.create(segment_rundir, showWarnings = FALSE, recursive = TRUE)
  file.create(file.path(segment_rundir, "README.txt"))
  segment_rundir_withid <- file.path(segment_rundir, runid)
  dir.create(segment_rundir_withid, showWarnings = FALSE, recursive = TRUE)

  segment_settings <- settings
  segment_settings[["outdir"]] <- segment_outdir
  segment_settings[["modeloutdir"]] <- segment_outdir
  segment_settings[["rundir"]] <- segment_rundir
  segment_settings[[c("run", "start.date")]] <- dstart
  segment_settings[[c("run", "end.date")]] <- dend
  segment_settings[[c("run", "inputs", "met", "path")]] <- met_segment_file
  segment_settings[[c("run", "inputs", "events")]] <- list(path = segment_eventfile)
  if (is.null(segment_settings[[c("model", "options")]])) {
    segment_settings[[c("model", "options")]] <- list()
  }

  if (isegment > 1) {
    # For isegment > 1, we restart from the *previous* segment's restart.out
    segment_settings[[c("model", "options", "RESTART_IN")]] <- restart_out
  }
  # ...and now, define a new restart.out for *this* segment
  restart_out <- file.path(segment_dir, "restart.out")
  segment_settings[[c("model", "options", "RESTART_OUT")]] <- restart_out

  # Write runs file
  writeLines(runid, file.path(segment_rundir, "runs.txt"))

  # TODO: Logic to get the trait values corresponding to the segment's PFT.
  # 1. Cross-reference crop_code against PFT
  # 2. Get traits from PFT posterior file.
  segment_traits <- list(list())

  config <- PEcAn.SIPNET::write.config.SIPNET(
    defaults = settings[["pfts"]],
    trait.values = segment_traits,
    settings = segment_settings,
    run.id = runid
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

# TODO: Concatenate the NetCDF files. This is annoyingly hard in R (unless I use `stars`?)

segment_ncfiles <- lapply(
  segments[["segment_dir"]],
  \(x) list.files(
    file.path(x, "out", "1"),
    pattern = "\\d+\\.nc",
    full.names = TRUE
  )
) |>
  do.call(what = c)

segment_files_byyear <- split(segment_ncfiles, factor(basename(segment_ncfiles)))
combined_outdir <- fs::dir_create(fs::path(outdir_root, "out"))
names(segment_files_byyear) <- fs::path(combined_outdir, names(segment_files_byyear))
results <- purrr::imap(segment_files_byyear, PEcAn.SIPNET::mergeNC)
