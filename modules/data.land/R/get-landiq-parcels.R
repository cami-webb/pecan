#' Get Parcel IDs from LandIQ
#'
#' @param design_points `data.frame` of coordinates to extract. Must contain
#' columns `id`, `lat`, and `lon`.
#' @param parcels_file Path to harmonized LandIQ parcels (GPKG) file
#'
#' @return `design_points` `data.frame` with harmonized LandIQ parcel_IDs
#' @export
get_landiq_parcel_ids <- function(design_points, parcels_file) {
  parcels_vect <- terra::vect(parcels_file)
  pts_sf <- design_points |>
    dplyr::select("id", "lat", "lon") |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(sf::st_crs(parcels_vect))
  dp_vect <- terra::vect(pts_sf)
  matched <- terra::intersect(dp_vect, parcels_vect)
  matched_sf <- sf::st_as_sf(matched) |>
    sf::st_drop_geometry()
  dp_with_parcels <- design_points |>
    dplyr::left_join(matched_sf, by = "id")
  dp_with_parcels
}
