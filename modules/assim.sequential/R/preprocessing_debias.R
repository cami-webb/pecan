#' Debias preprocessing utilities (internal)
#'
#' Helper functions for the SDA debias step in `sda.enkf.multisite()`:
#' (1) map observations to state columns; (2) collect covariates for the
#' correct year and align them to X's column layout; (3) build diagnostics.
#'
#' These helpers are pure (stateless) and do not use `settings`.
#'
#' @section Tidy-eval safety:
#' Uses `rlang::.data` in dplyr calls to avoid R CMD check notes.
#'
#' @keywords internal
#' @name debias_helpers
#' @importFrom dplyr filter right_join arrange
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @importFrom lubridate year
NULL

#' @rdname debias_helpers
#' @keywords internal
#' @author Shashank Ramachandran
# ---- Name mapping (edit if OBS names differ from STATE col names) ----
debias_name_map <- c(
  AGB   = "AbvGrndWood",
  LAI   = "LAI",
  SMP   = "SoilMoistFrac",
  SoilC = "TotSoilCarb"
)
#' @rdname debias_helpers
#' @keywords internal
# ---- Covariate accessor for a given observation datetime ----
# Ensures we have rows for all sites we actually use (from site_index)
debias_get_covariates_for_date <- function(covariates_df, obs_date, site_index) {
  yr <- lubridate::year(obs_date)
  sites_used <- unique(as.character(site_index))
  
  if (is.null(covariates_df)) {
    stop("covariates_df is NULL. Provide a table with columns: site, year, <features...>.")
  }
  
  df_year <- covariates_df |>
    dplyr::filter(.data$year == !!yr)
  
  # enforce presence & order of the sites we’re using this step
  df_year <- df_year |>
    dplyr::right_join(tibble::tibble(site = sites_used), by = "site") |>
    dplyr::arrange(.data$site)
  
  # sanity check
  if (any(is.na(df_year$year))) {
    missing_sites <- df_year$site[is.na(df_year$year)]
    stop("Missing covariates for sites in year ", yr, ": ",
         paste(missing_sites, collapse = ", "))
  }
  
  df_year
}
#' @rdname debias_helpers
#' @keywords internal
# ---- Expand covariates to match columns of X (row-per-column) ----
debias_cov_by_columns <- function(covariates_df, obs_date, site_index) {
  df_year <- debias_get_covariates_for_date(covariates_df, obs_date, site_index)
  feat_cols <- setdiff(names(df_year), c("site","year"))
  
  idx <- match(as.character(site_index), df_year$site)  # repeat per (site,var) column
  if (any(is.na(idx))) {
    stop("Internal error aligning covariates to site_index; check site labels.")
  }
  
  as.matrix(df_year[idx, feat_cols, drop = FALSE])
}
#' @rdname debias_helpers
#' @keywords internal
# ---- Build an observation vector aligned to X's columns ----
debias_obs_vec_for_time <- function(t_idx, site_index, col_vars, obs.mean, name_map = debias_name_map) {
  om  <- obs.mean[[t_idx]]
  out <- rep(NA_real_, length(col_vars))
  
  for (s in unique(site_index)) {
    vals <- om[[as.character(s)]]
    if (is.null(vals)) next
    
    if (!is.null(name_map)) {
      keep <- names(vals) %in% names(name_map)
      if (any(keep)) names(vals)[keep] <- unname(name_map[names(vals)[keep]])
    }
    
    v_here <- unique(col_vars[site_index == s])
    vnames <- intersect(names(vals), v_here)
    for (v in vnames) {
      idx <- which(site_index == s & col_vars == v)
      if (length(idx)) out[idx] <- as.numeric(vals[[v]][1])
    }
  }
  out
}
#' @rdname debias_helpers
#' @keywords internal
# ---- Diagnostics helpers (unchanged) ----
debias_build_comp_df <- function(site_index, col_vars, pre_mean, post_mean, obs_vec) {
  df <- data.frame(
    site = site_index,
    var  = col_vars,
    pre  = as.numeric(pre_mean),
    post = as.numeric(post_mean),
    obs  = as.numeric(obs_vec),
    stringsAsFactors = FALSE
  )
  df[order(df$var, df$site), ]
}
#' @rdname debias_helpers
#' @keywords internal
debias_rmse_by_var <- function(comp_df) {
  rmse <- function(a, b) sqrt(mean((a - b)^2, na.rm = TRUE))
  do.call(
    rbind,
    lapply(split(comp_df, comp_df$var), function(d) {
      data.frame(
        var       = d$var[1],
        rmse_pre  = rmse(d$pre,  d$obs),
        rmse_post = rmse(d$post, d$obs),
        stringsAsFactors = FALSE
      )
    })
  )
}



