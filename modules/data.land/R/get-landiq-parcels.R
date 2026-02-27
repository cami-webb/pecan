#' Get Parcel IDs from LandIQ
#'
#' @param design_points `data.frame` of coordinates to extract. Must contain
#' columns `id`, `lat`, and `lon`.
#' @param parcels_file Path to harmonized LandIQ parcels (GPKG) file
#'
#' @return `design_points` `data.frame` with harmonized LandIQ parcel_IDs
#' @export
get_landiq_parcel_ids <- function(design_points, parcels_file) {
  parcel_crs <- sf::st_layers(parcels_file)[["crs"]][[1]]
  pts_sf <- design_points |>
    dplyr::select("id", "lat", "lon") |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(parcel_crs)
  conn <- duckspatial::ddbs_create_conn()
  duckspatial::ddbs_write_vector(
    conn = conn,
    data = pts_sf,
    name = "design_points"
  )
  duckdb::dbSendQuery(
    conn,
    glue::glue(
      "
      CREATE TABLE merged AS
      SELECT dp.*, p.parcel_id,
      FROM design_points dp
      LEFT JOIN ST_Read('{parcels_file}', layer='parcels') p
      ON ST_Within(dp.geometry, p.geom)
      "
    )
  )
  dp_with_parcels <- duckspatial::ddbs_read_vector(
    conn = conn,
    name = "merged"
  ) |>
    sf::st_drop_geometry(dp_parcels) |>
    dplyr::right_join(design_points, by = "id")

  dp_with_parcels
}
