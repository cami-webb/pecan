#!/usr/bin/env Rscript

make_event_df <- function(
  parcel_waterbalance,
  outfile,
  n_ensemble = NULL,
  frac_uncertainty = 0.1
) {
  pw_sub <- parcel_waterbalance |>
    dplyr::filter(.data$irr > 0) |>
    dplyr::relocate("parcel_id", "date", "crop_name", "canopy_cover", "irr")

  irr_events <- pw_sub |>
    dplyr::mutate(
      crop_code = .data$crop_name,
      method = dplyr::case_when(
        .data$crop_code == "Rice" ~ "flood",
        TRUE ~ "canopy"
      )
    ) |>
    dplyr::select("parcel_id", "date", "amount_mm" = "irr", "method")

  if (is.null(n_ensemble)) {
    arrow::write_parquet(irr_events, outfile)
    return(invisible(outfile))
  }

  # Crude uncertainty propagation. We apply a uniform uncertainty multiplier
  # across the entire irrigation time series.
  unc_table <- irr_events |>
    dplyr::distinct(.data$parcel_id) |>
    dplyr::mutate(
      unc_multi = purrr::map(
        .data$parcel_id,
        ~rnorm(n_ensemble, 1.0, frac_uncertainty)
      ),
      ens_id = purrr::map(.data$unc_multi, seq_along)
    ) |>
    tidyr::unnest(c("unc_multi", "ens_id")) |>
    dplyr::mutate(ens_id = sprintf("irr_ens_%03d", .data$ens_id))

  irr_events_unc <- irr_events |>
    dplyr::left_join(
      unc_table,
      by = "parcel_id",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      amount_mm = .data$amount_mm * .data$unc_multi,
      .keep = "unused"
    ) |>
    dplyr::relocate("parcel_id", "ens_id", "date")

  arrow::write_parquet(irr_events_unc, outfile)
  invisible(outfile)
}
