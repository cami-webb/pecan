#' Merge CF-compliant meteorological NetCDF files
#'
#' Creates a new CF-compliant NetCDF file by merging selected variables
#' from a secondary CF NetCDF file into a primary CF NetCDF file.
#' Input files are never modified.
#'
#' @param primary_cf character. Path to primary CF NetCDF file
#' @param secondary_cf character. Path to secondary CF NetCDF file
#' @param vars character vector. CF variable names to merge
#' @param out_file character. Path to output NetCDF file
#' @param align_time logical. Whether to align time axes before merging
#'
#' @return character. Path to the newly created NetCDF file
#'
#' @noRd

  merge_cf_met_files <- function(
    primary_cf,
    secondary_cf,
    vars,
    out_file,
    align_time = TRUE
  ) {
  # TODO (#3605): Implement time-axis validation and alignment between
  # primary and secondary CF files (e.g. using cf2datetime() and logic
  # from align_met.R). This is intentionally deferred to a follow-up PR.

  # ---- open inputs (read-only)
  nc_primary <- ncdf4::nc_open(primary_cf)
  on.exit(ncdf4::nc_close(nc_primary), add = TRUE)

  nc_secondary <- ncdf4::nc_open(secondary_cf)
  on.exit(ncdf4::nc_close(nc_secondary), add = TRUE)


  # TODO (#3605): Replace copy-and-modify approach with explicit
  # CF-safe NetCDF construction. Output file is currently created by
  # copying the primary file and updating selected variables.
  # ---- create output file by copying base file
  file.copy(primary_cf, out_file, overwrite = TRUE)
  nc_out <- ncdf4::nc_open(out_file, write = TRUE)

  on.exit(ncdf4::nc_close(nc_out), add = TRUE)

  # ---- loop over variables to merge
  for (v in vars) {

    if (!(v %in% names(nc_primary$var)) || !(v %in% names(nc_secondary$var))) {
      next
    }

    primary_vals  <- ncdf4::ncvar_get(nc_primary, v)
    secondary_vals <- ncdf4::ncvar_get(nc_secondary, v)

    # only replace NA values
    replace_idx <- is.na(primary_vals) & !is.na(secondary_vals)

    if (any(replace_idx)) {
      primary_vals[replace_idx] <- secondary_vals[replace_idx]
      ncdf4::ncvar_put(nc_out, v, primary_vals)
    }
  }

  out_file
}
