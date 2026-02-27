#' Download CIMIS ETo data
#'
#' Read raw ETo.asc.gz directly from CIMIS spatial portal, add CRS, and save
#' locally as Cloud-optimized GeoTIFF. Outputs will be saved to
#' `<local_root_dir>/CIMIS-ETo-YYYY-MM-DD.tif`.
#'
#' @param date Date to download
#' @param local_root_dir Root directory for storing outputs.
#'
#' @return Path to saved TIF file (invisibly)
#' @export
download_cimis_et <- function(date, local_root_dir) {
  date_str <- format(date, "%Y/%m/%d")
  date_filename <- format(date, "%Y-%m-%d")

  base_url <- "https://spatialcimis.water.ca.gov/cimis"
  remote_path <- file.path(base_url, date_str, "ETo.asc.gz")
  vsicurl_path <- paste0("/vsigzip//vsicurl/", remote_path)

  tif_path <- file.path(
    local_root_dir,
    paste0("CIMIS-ETo-", date_filename, ".tif")
  )

  r <- terra::rast(vsicurl_path)
  terra::crs(r) <- "EPSG:3310"

  terra::writeRaster(r, tif_path, filetype = "COG", overwrite = TRUE)

  invisible(tif_path)
}

#' Extract CIMIS daily reference ETo values
#'
#' @param design_points `data.frame` of design points with columns
#' `location_id`, `lat`, and `lon`
#' @param download_missing If `TRUE` and the local COG is missing, download it.
#' If `FALSE` and the file is missing, throw an error.
#'
#' @inheritParams download_cimis_et
#'
#' @return `design_points` `data.frame` with additional columns `date`, and
#' `etref_mm_day` (reference ET, mm/day)
#' @export
extract_cimis_date <- function(
  design_points,
  date,
  local_root_dir,
  download_missing = FALSE
) {
  date_filename <- format(date, "%Y-%m-%d")
  tif_path <- file.path(
    local_root_dir,
    paste0("CIMIS-ET-", date_filename, ".tif")
  )

  if (!file.exists(tif_path)) {
    if (!download_missing) {
      stop("Missing file ", tif_path)
    }
    download_cimis_et(date, local_root_dir)
  }

  r <- terra::rast(tif_path)

  pts_sf <- sf::st_as_sf(design_points, coords = c("lon", "lat"), crs = 4326)
  pts_albers <- sf::st_transform(pts_sf, crs = 3310)
  coords <- sf::st_coordinates(pts_albers)

  vals <- terra::extract(r, coords)

  design_points |>
    dplyr::mutate(date = date, etref_mm_day = vals[, 1])
}

#' Extract CIMIS reference ET for multiple dates
#'
#' @param dates Sequence of dates for which to extract data
#' @inheritParams extract_cimis_date
#'
#' @return `design_points` `data.frame` extended with ETref data for all dates.
#' @export
extract_cimis_dates <- function(design_points, dates, ...) {
  df_list <- purrr::map(
    dates,
    purrr::possibly(extract_cimis_date, NULL, quiet = FALSE),
    design_points = design_points,
    ...
  )
  dplyr::bind_rows(df_list)
}
