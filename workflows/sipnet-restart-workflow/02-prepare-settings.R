#!/usr/bin/env Rscript

config <- config::get(file = "workflows/sipnet-restart-workflow/config.yml")
outdir_root <- config[["outdir_root"]]

outdir <- normalizePath(file.path(outdir_root, "output"))
modeloutdir <- file.path(outdir, "out")
dir.create(modeloutdir, showWarnings = FALSE, recursive = TRUE)
settings_rundir <- file.path(outdir, "run")
dir.create(settings_rundir, showWarnings = FALSE, recursive = TRUE)

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

metfiles <- file.path(config[["met_dir"]], site_id) |>
  list.files("ERA5\\..*\\.clim", full.names = TRUE) |>
  as.list()
names(metfiles) <- paste0("path", seq_along(metfiles))

icfiles <- file.path(config[["ic_dir"]], site_id) |>
  list.files("IC_site_.*\\.nc", full.names = TRUE) |>
  as.list()
names(icfiles) <- paste0("path", seq_along(icfiles))

pft_dir <- config[["pft_dir"]]
stopifnot(dir.exists(pft_dir))

pfts <- c("temperate.deciduous", "grass", "annual_crop") |>
  purrr::map(~list(
    name = .x,
    posterior.files = file.path(pft_dir, .x, "post.distns.Rdata"),
    outdir = paste0(file.path(pft_dir, .x), "/")
  )) |>
  c(list(list(name = "soil", outdir = file.path(pft_dir, "soil/"))))
names(pfts) <- rep("pft", length(pfts))

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
    poolinitcond = list(method = "sampling")
  ),
  start.year = lubridate::year(start_date),
  end.year = lubridate::year(end_date)
)

settings_raw <- PEcAn.settings::as.Settings(list(
  outdir = outdir,
  modeloutdir = modeloutdir,
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
      met = list(path = metfiles),
      poolinitcond = list(path = icfiles),
      events = list(path = sipnet_eventfile)
    )
  ),
  host = list(
    name = "localhost"
  )
))

PEcAn.settings::write.settings(settings_raw, "settings.xml", outdir_root)
