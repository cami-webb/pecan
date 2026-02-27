#' Calculate water balance for a time series at a single site
#'
#' This is the core water balance calculation that operates on primitive
#' numeric vectors for easy testing and debugging. Each input is a time series
#' of daily values for a single location (one date per row). The units for all
#' quantities are arbitrary, but they should be consistent (distance / time;
#' e.g., most commonly, mm/day).
#'
#' @param et Vector of evapotranspiration values (distance / time)
#' @param precip Vector of precipitation values (distance / time)
#' @param whc Water holding capacity (distance)
#' @param w_min_frac Fraction of WHC for minimum water level
#' @return List with vectors: W_t (water balance), irr (irrigation), runoff
#' @export
calc_water_balance <- function(et, precip, whc, w_min_frac) {
  n <- length(et)
  if (length(precip) != n) {
    stop("et and precip must have the same length")
  }

  w_min <- w_min_frac * whc
  field_capacity <- whc / 2

  #nolint start: object_name_linter
  W_t <- numeric(n)
  W0_t <- numeric(n)
  irr <- numeric(n)
  runoff <- numeric(n)

  W_t[1] <- field_capacity

  for (t in seq_len(n)) {
    if (t == 1) {
      W_prev <- field_capacity
    } else {
      W_prev <- W_t[t - 1]
    }

    W0_t[t] <- W_prev + precip[t] - et[t]

    irr[t] <- max(w_min - W0_t[t], 0)

    runoff[t] <- max(W0_t[t] - whc, 0)

    W_t[t] <- W_prev + precip[t] + irr[t] - et[t] - runoff[t]
  }

  # nolint end: object_name_linter

  list(
    W_t = W_t,
    irr = irr,
    runoff = runoff
  )
}

#' Apply water balance calculations to a data frame with multiple sites
#'
#' Groups by location and applies calc_water_balance to each group. Unlike
#' `calc_water_balance`, the units here *do* matter -- they should be `mm_day`.
#'
#' @param df Data frame with columns: `date`, `location_id`, `etc_mm_day`,
#' `precip_mm_day`, and `whc_min_frac`
#' @param idcol Column name for grouping (typically, `location_id`, `parcel_id` or similar)
#' @param whc_mm Water holding capacity (mm)
#' @return Data frame with added columns: `W_t`, `irr`, `runoff`
#' @export
apply_water_balance <- function(df, idcol, whc_mm = 500) {
  need_cols <- c("etc_mm_day", "precip_mm_day", "date")
  missing_cols <- need_cols[!(need_cols %in% colnames(df))]
  default_whc_min_frac <- 0.375
  if (length(missing_cols) > 0) {
    PEcAn.logger::logger.severe(
      "Missing the following required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if ("whc_min_frac" %in% colnames(df)) {
    w_min_frac <- df$whc_min_frac
    n_na <- sum(is.na(w_min_frac))
    if (n_na > 0) {
      PEcAn.logger::logger.warn(
        sprintf(
          "whc_min_frac has %d NA values. Replacing with default = %.3f",
          n_na,
          default_whc_min_frac
        )
      )
      w_min_frac <- tidyr::replace_na(w_min_frac, default_whc_min_frac)
    }
  } else {
    PEcAn.logger::logger.warn(
      "whc_min_frac column not found in input data. Using default = 0.375"
    )
    w_min_frac <- default_whc_min_frac
  }

  df |>
    dplyr::arrange(.data[[idcol]], .data$date) |>
    dplyr::mutate(
      year = as.integer(format(.data$date, "%Y")),
      week = as.integer(format(.data$date, "%U")),
      day_of_year = as.integer(format(.data$date, "%j")),
      results = tibble::as_tibble(calc_water_balance(
        et = .data$etc_mm_day,
        precip = .data$precip_mm_day,
        whc = whc_mm,
        w_min_frac = w_min_frac
      )),
      .by = dplyr::all_of(idcol)
    ) |>
    tidyr::unpack(results)
}
