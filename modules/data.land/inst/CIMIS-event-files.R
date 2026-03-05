#' ---
#' title: "Example workflow generating SIPNET event files from CIMIS and CHIRPS data"
#' author: "Alexey N. Shiklomanov"
#' ---

if (interactive()) {
  devtools::load_all("modules/data.land")
} else {
  library(PEcAn.data.land)
}

#' Define paths to relevant external data files
design_points_path <- "~/projects/cimis-to-irrigation/design_points.csv"
cimis_eto_cog_path <- "~/data/CIMIS-ETo-COG"
parcels_path <- "~/data/LandIQ-harmonized-v3/parcels.gpkg"
crops_path <- "~/data/LandIQ-harmonized-v3/crops_all_years.parq"

#' Start from a range of dates (2020 to present) and locations (`design_points.csv`).

dates <- seq.Date(as.Date("2020-03-01"), as.Date("2020-11-30"), "day")
design_points <- readr::read_csv(design_points_path) |>
  head(10)

#' # CIMIS ETref
#'
#' For each site, extract its reference ETref from the CIMIS data.

etref <- design_points |>
  extract_cimis_dates(
    dates,
    cimis_eto_cog_path,
    .progress = TRUE
  )

#' # CHIRPS Precipitation
#'
#' Also, extract precipitation from CHIRPS v2.

precip <- extract_chirps_remote(design_points, dates)

#' # BIS Kc coefficients
#'
#' For each site, get LandIQ parcel and crop data.

dp_with_crops <- get_landiq(
  design_points,
  parcels_file = parcels_path,
  crops_file = crops_path
) |>
  tibble::as_tibble()

#' Map `CLASS/SUBCLASS` to `crop_name` using `bism_kc_by_crop`.
#'
#' **NOTE:** Some LandIQ classes/subclasses map onto *multiple BISM crop types*.

bism_kc_by_crop |>
  dplyr::summarize(
    n_unique = dplyr::n(),
    .by = c("landiq_class", "landiq_subclass")
  ) |>
  dplyr::filter(n_unique > 1) |>
  dplyr::left_join(bism_kc_by_crop) |>
  dplyr::summarize(
    crops = paste(crop_name, collapse = ", "),
    .by = c("landiq_class", "landiq_subclass", "n_unique")
  )

#' So, below, we introduce a **HACK** to select just the first crop in any of these groups.
#' The more correct fix is to do some kind of averaging later.

bism_crop_unique <- bism_kc_by_crop |>
  dplyr::distinct(landiq_class, landiq_subclass, crop_name) |>
  # WARNING: Hack here!
  dplyr::slice(1, .by = c("landiq_class", "landiq_subclass"))
design_point_crops <- dp_with_crops |>
  dplyr::left_join(
    bism_crop_unique,
    by = c("CLASS" = "landiq_class", "SUBCLASS" = "landiq_subclass")
  )

#' For demonstration purposes, we will expand this naively using `tidyr::fill` and hard-code dates for the 4 seasons to January 1, April 1, July 1, October 1.
#' In reality, you would resolve these more finely using phenology data (e.g., from remote sensing).

fill_season <- function(year, season) {
  if (season == 1) {
    start <- lubridate::make_date(year, 1, 1)
    end <- lubridate::make_date(year, 3, 31)
  } else if (season == 2) {
    start <- lubridate::make_date(year, 4, 1)
    end <- lubridate::make_date(year, 6, 30)
  } else if (season == 3) {
    start <- lubridate::make_date(year, 7, 1)
    end <- lubridate::make_date(year, 9, 30)
  } else if (season == 4) {
    start <- lubridate::make_date(year, 10, 1)
    end <- lubridate::make_date(year, 12, 31)
  }
  seq.Date(start, end, "day")
}

dp_crops_filled <- design_point_crops |>
  dplyr::filter(!is.na(season)) |>
  tidyr::fill(
    "CLASS",
    "SUBCLASS",
    "crop_name",
    .direction = "downup",
    .by = "parcel_id"
  ) |>
  dplyr::mutate(date = purrr::map2(year, season, fill_season)) |>
  tidyr::unnest(date) |>
  dplyr::filter(date %in% !!dates)

#' Identify and warn about parcels with no matching BIS crop.

missing_crops <- dp_crops_filled |> dplyr::filter(is.na(crop_name))
if (nrow(missing_crops) > 0) {
  missing_crop_strs <- missing_crops |>
    dplyr::distinct(CLASS, SUBCLASS) |>
    dplyr::mutate(string = glue::glue("CLASS: {CLASS} SUBCLASS: {SUBCLASS}")) |>
    dplyr::pull(string)
  missing_crop_str <- sprintf("[%s]", paste(missing_crop_strs, collapse = "; "))
  warning(
    "Skipping ",
    nrow(missing_crops),
    " rows with no matching BIS crop. Relevant pairs are: ",
    missing_crop_str
  )
}

dp_with_cropname <- dp_crops_filled |>
  dplyr::filter(!is.na(crop_name)) |>
  dplyr::left_join(
    crop_whc |> dplyr::select("crop_name", "whc_min_frac", "rooting_depth_m"),
    by = "crop_name"
  )

#' # SSURGO Soil Data
#'
#' Calculate site-specific water holding capacity (WHC) from SSURGO soil data and crop rooting depth.

calc_effective_awc <- function(
  hzdept_r_cm,
  hzdepb_r_cm,
  awc_r,
  rooting_depth_cm
) {
  # Clip each horizon to the rooting depth
  effective_top <- pmin(hzdept_r_cm, rooting_depth_cm)
  effective_bottom <- pmin(hzdepb_r_cm, rooting_depth_cm)
  thickness_cm <- pmax(0, effective_bottom - effective_top)

  # awc_r is cm water / cm soil, so multiply by thickness to get cm water.
  # Convert cm water to mm water by multiplying by 10.
  sum(awc_r * thickness_cm, na.rm = TRUE) * 10
}

# 1. Get mukeys for all design points
design_points_sf <- design_points |>
  dplyr::distinct(id, lon, lat)

mukeys_list <- purrr::map2(
  design_points_sf$lon,
  design_points_sf$lat,
  ~ ssurgo_mukeys_point(point = c(.x, .y), distance = 20)
)

# 2. Query gSSURGO for soil data
all_mukeys <- unique(unlist(mukeys_list))
soil_raw <- gSSURGO.Query(
  mukeys = all_mukeys,
  fields = c("chorizon.awc_r", "chorizon.hzdept_r", "chorizon.hzdepb_r")
)

# 3. Calculate effective WHC for each site-crop combination
# We use the dominant soil component for each map unit.
soil_dominant <- soil_raw |>
  dplyr::filter(cokey == cokey[which.max(comppct_r)], .by = "mukey")

dp_with_whc <- dp_with_cropname |>
  dplyr::mutate(mukey = mukeys_list[match(id, design_points_sf$id)]) |>
  tidyr::unnest(mukey) |>
  dplyr::mutate(mukey = as.numeric(mukey)) |>
  dplyr::left_join(
    soil_dominant,
    by = "mukey",
    relationship = "many-to-many"
  ) |>
  dplyr::summarize(
    whc_mm = calc_effective_awc(
      hzdept_r,
      hzdepb_r,
      awc_r,
      rooting_depth_cm = rooting_depth_m[[1]] * 100
    ),
    .by = c("id", "parcel_id", "date", "crop_name", "whc_min_frac")
  ) |>
  # Fallback to default if WHC is 0 or NA
  dplyr::mutate(whc_mm = dplyr::if_else(whc_mm > 0, whc_mm, 500, missing = 500))

#' # Join with ETo data
#'
#' Join with ETref data.

dp_with_eto <- dp_with_whc |>
  dplyr::left_join(
    (etref |> dplyr::select("id", "date", "etref_mm_day")),
    by = c("id", "date")
  )

#' Calculate ETc directly using eto_to_etc_bism. Group by crop_name and apply since eto_to_etc_bism takes a single crop at a time.

dp_with_etc <- dp_with_eto |>
  dplyr::mutate(
    etc_mm_day = eto_to_etc_bism(
      eto = etref_mm_day,
      crop_name = crop_name[[1]],
      date = date
    ),
    .by = "crop_name"
  ) |>
  dplyr::select(
    dplyr::any_of(c("id", "parcel_id", "lat", "lon")),
    "date",
    "etc_mm_day",
    "whc_min_frac",
    "whc_mm"
  )

#' Handle multi-crop parcels (double-cropping) - placeholder logic that warns and averages ETc values.

resolve_multicrop <- function(etc_data, id_col = "id", date_col = "date") {
  id_sym <- rlang::sym(id_col)
  date_sym <- rlang::sym(date_col)

  multicrop_counts <- etc_data |>
    dplyr::add_count(!!id_sym, !!date_sym, name = "n") |>
    dplyr::filter(.data$n > 1) |>
    dplyr::summarize(
      n_multicrop = dplyr::n_distinct(!!id_sym, !!date_sym),
      .groups = "drop"
    )

  if (multicrop_counts$n_multicrop > 0) {
    message(
      "Multi-crop parcels: ",
      multicrop_counts$n_multicrop,
      " date-parcel combinations have multiple crops. Averaging ETc and WHC values."
    )
  }

  etc_data |>
    dplyr::group_by(!!id_sym, !!date_sym) |>
    dplyr::summarize(
      etc_mm_day = mean(.data$etc_mm_day, na.rm = TRUE),
      whc_min_frac = mean(.data$whc_min_frac, na.rm = TRUE),
      whc_mm = mean(.data$whc_mm, na.rm = TRUE),
      .groups = "drop"
    )
}

dp_with_etc <- resolve_multicrop(dp_with_etc)

#' Join with precipitation data (inner_join to ensure matching dates).

dp_crops_all <- dp_with_etc |>
  dplyr::inner_join(precip, by = c("id", "date")) |>
  dplyr::select(c(
    "id",
    "lat",
    "lon",
    "date",
    "etc_mm_day",
    "precip_mm_day",
    "whc_min_frac",
    "whc_mm"
  ))

#' # Calculate water balance

dpwb <- apply_water_balance(dp_crops_all, "id")

#' Check crop evapotranspiration values are reasonable.

etc_summary <- dp_crops_all |>
  dplyr::summarize(
    etc_min = min(.data$etc_mm_day, na.rm = TRUE),
    etc_max = max(.data$etc_mm_day, na.rm = TRUE),
    etc_mean = mean(.data$etc_mm_day, na.rm = TRUE),
    .by = "id"
  )
print(etc_summary)

#' Check that water balance calculations are reasonable.

wb_summary <- dpwb |>
  dplyr::group_by(.data$id) |>
  dplyr::summarize(
    irr_total = sum(.data$irr, na.rm = TRUE),
    irr_max = max(.data$irr, na.rm = TRUE),
    irr_mean = mean(.data$irr, na.rm = TRUE),
    runoff_total = sum(.data$runoff, na.rm = TRUE),
    W_t_min = min(.data$W_t, na.rm = TRUE),
    W_t_max = max(.data$W_t, na.rm = TRUE),
    .groups = "drop"
  )
print(wb_summary)

#' Check for other issues.

if (any(wb_summary$irr_max < 0)) {
  warning("Negative irrigation values detected!")
} else {
  message("Irrigation values are non-negative")
}

if (any(wb_summary$W_t_min < 0)) {
  warning("Negative soil water values detected!")
} else {
  message("Soil water values are non-negative")
}

#' Seasonal variation check - irrigation should be higher in summer.

monthly_irr <- dpwb |>
  dplyr::mutate(month = lubridate::month(.data$date)) |>
  dplyr::group_by(.data$month) |>
  dplyr::summarize(irr_mean = mean(.data$irr, na.rm = TRUE), .groups = "drop")
print(monthly_irr)

#' # Plot results

library(ggplot2)
dpwb |>
  ggplot() +
  aes(x = date, y = irr, color = id) +
  geom_line() +
  labs(title = "Irrigation Requirements by Site", y = "Irrigation (mm/day)")

#' # Write event files
#'
#' Example of a single event data frame.

dpwb |>
  dplyr::filter(id == id[[1]]) |>
  create_event_file()

#' Write all event files.

outdir <- tempfile(pattern = "events_")
dir.create(outdir)
dpwb |>
  dplyr::group_nest(.data$id) |>
  dplyr::mutate(
    fname = purrr::map2(
      id,
      data,
      \(id, dat) {
        readr::write_delim(
          create_event_file(dat),
          file.path(outdir, glue::glue("{id}_events.txt")),
          delim = " ",
          col_names = FALSE
        )
      }
    )
  )

fnames <- list.files(outdir, full.names = TRUE)
cat(readr::read_file(fnames[[1]]))
