#' Calculate water balance for a time series at a single site
#'
#' This is the core water balance calculation that operates on primitive
#' numeric vectors for easy testing and debugging. Each input is a time series
#' of daily values for a single location (one date per row).
#'
#' @param et Vector of evapotranspiration values (mm/day)
#' @param precip Vector of precipitation values (mm/day)
#' @param whc Water holding capacity (mm), default 500
#' @param w_min_frac Fraction of WHC for minimum water level, default 0.375 is
#' halfway between the recommended defaults for woody PFTs (0.25) and annuals (0.50)
#' @return List with vectors: W_t (water balance), irr (irrigation), runoff
#' @export
calc_water_balance <- function(et, precip, whc = 500, w_min_frac = 0.375) {
  n <- length(et)
  if (length(precip) != n) {
    stop("et and precip must have the same length")
  }

  w_min <- w_min_frac * whc
  field_capacity <- whc / 2

  W_t <- numeric(n) #nolint: object_name_linter
  W0_t <- numeric(n) #nolint: object_name_linter
  irr <- numeric(n)
  runoff <- numeric(n)

  W_t[1] <- field_capacity #nolint: object_name_linter

  for (t in seq_len(n)) {
    if (t == 1) {
      W_prev <- field_capacity #nolint: object_name_linter
    } else {
      W_prev <- W_t[t - 1] #nolint: object_name_linter
    }

    W0_t[t] <- W_prev + precip[t] - et[t] #nolint: object_name_linter

    irr[t] <- max(w_min - W0_t[t], 0)

    runoff[t] <- max(W0_t[t] - whc, 0)

    W_t[t] <- W_prev + precip[t] + irr[t] - et[t] - runoff[t] #nolint: object_name_linter
  }

  list(
    W_t = W_t,
    irr = irr,
    runoff = runoff
  )
}

#' Apply water balance calculations to a data frame with multiple sites
#'
#' Groups by location and applies calc_water_balance to each group.
#'
#' @param df Data frame with columns: date, location_id, et_mm_day, precip_mm_day
#' @param idcol Column name for grouping (typically, `location_id`, `parcel_id` or similar)
#' @param whc Water holding capacity (mm)
#' @return Data frame with added columns: W_t, irr, runoff
#' @export
apply_water_balance <- function(df, idcol, whc = 500) {
  need_cols <- c("etc_mm_day", "precip_mm_day", "date")
  missing_cols <- need_cols[!(need_cols %in% colnames(df))]
  if (length(missing_cols) > 0) {
    PEcAn.logger::logger.severe(
      "Missing the following required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  df |>
    dplyr::arrange(.data[[idcol]], .data$date) |>
    dplyr::mutate(
      year = as.integer(format(.data$date, "%Y")),
      week = as.integer(format(.data$date, "%U")),
      day_of_year = as.integer(format(.data$date, "%j")),
      results = tibble::as_tibble(calc_water_balance(
        .data$etc_mm_day,
        .data$precip_mm_day,
        whc = whc
      )),
      .by = dplyr::all_of(idcol)
    ) |>
    tidyr::unpack(results)
}
