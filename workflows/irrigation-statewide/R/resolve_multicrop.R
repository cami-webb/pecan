#' Average ETc and WHC across multi-crop parcels (double-cropping hack).
resolve_multicrop <- function(etc_data, id_col = "id", date_col = "date") {
  id_sym   <- rlang::sym(id_col)
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
      etc_mm_day   = mean(.data$etc_mm_day,   na.rm = TRUE),
      whc_min_frac = mean(.data$whc_min_frac, na.rm = TRUE),
      whc_mm       = mean(.data$whc_mm,       na.rm = TRUE),
      .groups = "drop"
    )
}
