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

  crops <- arrow::open_dataset(crops_file) |>
    dplyr::filter(.data$parcel_id %in% unique(dp_with_parcels[["parcel_id"]])) |> 
    dplyr::select("parcel_id", "year", "season", "CLASS", "SUBCLASS") |>
    dplyr::collect()

  dp_with_crops <- dp_with_parcels |>
    dplyr::left_join(crops, by = "parcel_id") |>
    tibble::as_tibble()

  dp_with_crops
}
