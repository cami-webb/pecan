#' Generate site-year covariates from yearly GeoTIFF stacks
#'
#' Scans `cov_dir` for files named `covariates_YYYY.tiff`, extracts raster values
#' at site coordinates from `settings$run`, and returns one long tibble with all
#' years stacked.
#'
#' @details
#' - Expected filename pattern: `covariates_YYYY.tiff` (one multi-layer stack per year).
#' - Point CRS defaults to EPSG:4326 (lon/lat). If the rasters are in a different CRS,
#'   `terra::extract()` will handle on-the-fly reprojection.
#' - Output has columns `site`, `year`, and one column per covariate layer in the TIFF.
#'   Rows are ordered by the order of sites in `settings$run` and by year ascending.
#'
#' @param settings PEcAn settings list; must contain `settings$run[[i]]$site$id/lat/lon`.
#' @param cov_dir  Directory containing files named like `"covariates_YYYY.tiff"`.
#' @param crs      CRS for site coordinates, default `"EPSG:4326"`.
#'
#' @return A tibble with columns: `site`, `year`, and one column per covariate layer.
#'
#' @examples
#' \dontrun{
#' cov_dir <- "/path/to/covariates"
#' cov_df  <- generate_covariates_df(settings, cov_dir)
#' dplyr::glimpse(cov_df)
#' }
#'
#' @importFrom purrr map_df map2_dfr
#' @importFrom dplyr mutate select filter arrange
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_extract
#' @importFrom terra vect rast extract
#' @importFrom rlang .data
#' @export
generate_covariates_df <- function(settings, cov_dir, crs = "EPSG:4326") {
  # --- checks ---
  if (!dir.exists(cov_dir)) stop("`cov_dir` does not exist: ", cov_dir)
  if (is.null(settings$run) || length(settings$run) == 0) {
    stop("`settings$run` is missing or empty; cannot build site coordinates.")
  }
  
  # 1) Build site_coords and coerce lon/lat to numeric
  site_coords <- purrr::map_df(settings$run, ~ tibble::tibble(
    site = as.character(.x$site$id),
    lat  = suppressWarnings(as.numeric(.x$site$lat)),
    lon  = suppressWarnings(as.numeric(.x$site$lon))
  ))
  if (anyNA(site_coords$lat) || anyNA(site_coords$lon)) {
    bad <- dplyr::filter(site_coords, is.na(.data$lat) | is.na(.data$lon))$site
    stop("Found non-numeric lat/lon for sites: ", paste(bad, collapse = ", "))
  }
  
  # 2) Create the SpatVector
  coords_mat <- as.matrix(site_coords[, c("lon", "lat")])
  pts        <- terra::vect(coords_mat, type = "points", crs = crs)
  pts$site   <- site_coords$site
  
  # 3) Discover years from filenames
  tif_files <- list.files(cov_dir, pattern = "^covariates_\\d{4}\\.tiff$", full.names = TRUE)
  if (length(tif_files) == 0) {
    stop("No files matched '^covariates_\\d{4}\\.tiff$' in: ", cov_dir)
  }
  years <- as.integer(stringr::str_extract(basename(tif_files), "\\d{4}"))
  ord   <- order(years)
  tif_files <- tif_files[ord]
  years     <- years[ord]
  
  # 4) Per-year extractor
  extract_year <- function(tif_path, year) {
    r    <- terra::rast(tif_path)
    vals <- terra::extract(r, pts)
    
    # Drop "ID" column if present (wherever it appears)
    if ("ID" %in% names(vals)) {
      vals <- vals[, setdiff(names(vals), "ID"), drop = FALSE]
    }
    
    out <- tibble::as_tibble(vals)
    if (nrow(out) != nrow(site_coords)) {
      stop("Row mismatch for year ", year, ": expected ", nrow(site_coords),
           " rows but got ", nrow(out), ". Check CRS/coordinates.")
    }
    
    dplyr::mutate(
      out,
      site = site_coords$site,
      year = as.integer(year)
    ) |>
      dplyr::select(.data$site, .data$year, dplyr::everything())
  }
  
  # 5) Map over all years and return a single tibble
  purrr::map2_dfr(tif_files, years, extract_year)
}
