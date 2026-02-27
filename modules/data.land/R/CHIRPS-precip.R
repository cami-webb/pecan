#' Extract CHIRPS Precipitation Data from Remote NetCDF
#'
#' Downloads and extracts daily precipitation data from the CHIRPS (Climate
#' Hazards group InfraRed Precipitation with Station data) dataset via remote
#' NetCDF file access using vsicurl.
#'
#' @param design_points A data frame or tibble containing columns `lon` and `lat`
#'   specifying the geographic coordinates of points to extract precipitation for.
#' @param dates A vector of dates or date-time objects specifying the days for which
#'   to extract precipitation data.
#' @returns A modified version of `design_points` with new rows added for each date,
#'   plus two new columns:
#'   \item{date}{The date of the extracted data (same as the input `date`).}
#'   \item{precip_mm_day}{Precipitation in millimeters for the specified day.}
#' @examples
#' \dontrun{
#' pts <- tibble::tibble(lon = c(-120, -110), lat = c(35, 40), site_id = 1:2)
#' result <- extract_chirps_remote(pts, as.Date(c("2020-06-15", "2021-06-15")))
#' }
extract_chirps_remote <- function(design_points, dates) {
  CHIRPS_REMOTE_ROOT <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/netcdf/p05"

  dates <- lubridate::as_date(dates)
  years <- lubridate::year(dates)

  result_list <- list()

  for (yr in unique(years)) {
    dates_yr <- dates[years == yr]
    day_of_year_yr <- lubridate::yday(dates_yr)

    url <- glue::glue("{CHIRPS_REMOTE_ROOT}/chirps-v2.0.{yr}.days_p05.nc")
    vsicurl_path <- paste0("/vsicurl/", url)

    # Suppress only the "no extent" warning. We set the extent on the next
    # line. Other warnings should still fire.
    withCallingHandlers({
      r <- terra::rast(vsicurl_path)
    },
    warning = function(w) {
      if (grepl("unknown extent", w$message)) {
        invokeRestart("muffleWarning")
      }
    })
    terra::ext(r) <- c(-180, 180, -50, 50)
    terra::crs(r) <- "EPSG:4326"

    r_days <- r[[day_of_year_yr]]

    pts <- as.matrix(design_points[, c("lon", "lat")])
    vals <- terra::extract(r_days, pts)
    vals_vec <- as.vector(t(t(vals)))

    result_list[[as.character(yr)]] <- design_points |>
      tidyr::expand_grid(date = dates_yr) |>
      dplyr::mutate(precip_mm_day = vals_vec)
  }

  dplyr::bind_rows(result_list)
}
