#' Get LandIQ parcels and crop data
#'
#' @param design_points `data.frame` of coordinates to extract. Must contain
#' columns `id`, `lat`, and `lon`.
#' @param parcels_file Path to harmonized LandIQ parcels (GPKG) file
#' @param crops_file Path to LandIQ crops parquet file
#'
#' @return `design_points` `data.frame` with LandIQ parcel IDs, year, season, CLASS, and SUBCLASS
#' @export
get_landiq <- function(design_points, parcels_file, crops_file) {
  dp_with_parcels <- get_landiq_parcel_ids(design_points, parcels_file)

  crops <- arrow::read_parquet(crops_file) |>
    dplyr::semi_join(dp_with_parcels, by = "parcel_id") |>
    dplyr::mutate(
      dplyr::across(c("CLASS", "SUBCLASS"), ~ dplyr::na_if(.x, "**")),
      SUBCLASS = as.integer(.data$SUBCLASS)
    ) |>
    dplyr::select("parcel_id", "year", "season", "CLASS", "SUBCLASS")

  dp_with_crops <- dp_with_parcels |>
    dplyr::left_join(crops, by = "parcel_id")

  dp_with_crops
}
