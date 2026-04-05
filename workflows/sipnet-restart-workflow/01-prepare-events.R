#!/usr/bin/env Rscript

config <- config::get(file = "workflows/sipnet-restart-workflow/config.yml")

# Pick a parcel from irrigation
pid <- config[["parcel_id"]]
site_id <- config[["site_id"]]

# Find the closest design point to that parcel to use existing met
if (is.null(site_id)) {
  parcel_path <- config[["parcel_path"]]
  parcel <- sf::read_sf(
    parcel_path,
    query = glue::glue("SELECT * FROM parcels WHERE parcel_id = {pid}")
  )

  dp_path <- config[["dp_path"]]
  design_points <- read.csv(dp_path) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(sf::st_crs(parcel))
  dp_idx <- sf::st_nearest_feature(parcel, design_points)
  site_id <- design_points[dp_idx, ][["id"]]
}

# Make the events.json
planting <- fs::dir_ls(
  config[["planting_events_dir"]],
  regexp = "planting_statewide_.*\\.parquet"
) |>
  arrow::open_dataset() |>
  dplyr::collect() |>
  dplyr::filter(.data$site_id == as.character(.env$pid)) |>
  dplyr::mutate(date = as.Date(.data$date)) |>
  tibble::as_tibble()

code_pft_mapping <- planting |>
  dplyr::distinct(crop_code = .data$code, .data$PFT)

planting_events <- planting |>
  dplyr::select(
    "site_id", "event_type", "date",
    "crop_code" = "code",
    "leaf_c_kg_m2" = "C_LEAF",
    "wood_c_kg_m2" = "C_STEM",
    "fine_root_c_kg_m2" = "C_FINEROOT",
    "coarse_root_c_kg_m2" = "C_COARSEROOT",
    "leaf_n_kg_m2" = "N_LEAF",
    "wood_n_kg_m2" = "N_STEM",
    "fine_root_n_kg_m2" = "N_FINEROOT",
    "coarse_root_n_kg_m2" = "N_COARSEROOT"
  )

# Harvest
mslsp_path <- config[["mslsp_path"]]
phenology <- fs::dir_ls(mslsp_path, glob = "*.parquet") |>
  arrow::open_dataset() |>
  dplyr::filter(.data$parcel_id == .env$pid, !is.na(.data$mslsp_cycle)) |>
  dplyr::collect() |>
  tibble::as_tibble() |>
  dplyr::arrange(.data$year, .data$mslsp_cycle) |>
  dplyr::relocate(
    "year", "mslsp_cycle", dplyr::starts_with("landiq_"),
  )

# Dummy values for testing
harvest_events <- phenology |>
  dplyr::mutate(
    event_type = "harvest",
    site_id = as.character(.data$parcel_id),
    frac_above_removed_0to1 = 0.85
  ) |>
  dplyr::select(
    "site_id", "event_type", "date" = mslsp_OGMn, "frac_above_removed_0to1"
  )

start_date <- min(planting$date)
end_date <- max(harvest_events$date)

irrigation_path <- config[["irrigation_path"]]

irrigation_events <- arrow::open_dataset(irrigation_path) |>
  dplyr::filter(
    .data$parcel_id == .env$pid,
    .data$ens_id == "irr_ens_001"
  ) |>
  dplyr::select(-"ens_id") |>
  dplyr::collect() |>
  tibble::as_tibble() |>
  dplyr::filter(.data$date <= .env$end_date) |>
  dplyr::mutate(
    event_type = "irrigation",
    site_id = as.character(.data$parcel_id),
    .keep = "unused"
  ) |>
  dplyr::relocate("site_id", "event_type", "date")

make_event_list <- function(df) {
  df2list <- function(df) {
    as.list(df) |> purrr::list_transpose()
  }
  df |>
    tidyr::nest(.by = "site_id", .key = "events") |>
    dplyr::mutate(events = purrr::map(.data$events, df2list))
}

planting_n <- make_event_list(planting_events)
harvest_n <- make_event_list(harvest_events)
irrigation_n <- make_event_list(irrigation_events)
all_events <- dplyr::bind_rows(planting_n, harvest_n, irrigation_n) |>
  dplyr::summarize(events = list(purrr::list_c(.data$events)), .by = "site_id") |>
  dplyr::mutate(pecan_events_version = "0.1.1", .before = "site_id")

outdir_root <- fs::dir_create(config[["outdir_root"]])
events_json_file <- fs::path(outdir_root, "events.json")
jsonlite::write_json(all_events, events_json_file, pretty = TRUE, auto_unbox = TRUE)
