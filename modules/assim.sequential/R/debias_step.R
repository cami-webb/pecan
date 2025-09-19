#' Apply residual debiasing for a single SDA time step (internal)
#'
#' Trains/updates per-variable residual models using data from t-1 and predicts
#' residuals at t, then mean-shifts the ensemble accordingly. Also returns
#' diagnostics and learner weights for logging.
#'
#' @param t Integer time index (>= 2 when debiasing runs).
#' @param obs.t Character time label used for diagnostics lists (e.g., "2012-12-31").
#' @param X Numeric matrix (ensembles x columns) of current raw forecast at time t.
#' @param raw_prev Named/numeric vector of raw ensemble **mean** at t-1 (length = ncol(X)).
#' @param raw_mean_t Named/numeric vector of raw ensemble **mean** at t   (length = ncol(X)).
#' @param site_index Character vector (length = ncol(X)) mapping columns to site IDs (attr(X, "Site")).
#' @param col_vars Character vector (length = ncol(X)) of state-variable names per column.
#' @param obs.times POSIXct vector of observation timestamps (length >= t).
#' @param obs.mean Observation list as used elsewhere (time-indexed, then site-indexed).
#' @param covariates_df Long tibble with columns site, year, <features...>.
#' @param py Reticulate module that implements: train_full_model, predict_residual,
#'   get_model_weights, has_model.
#' @param train_buf Environment used as a per-variable growing training buffer (modified in place).
#' @param name_map Optional named character vector mapping OBS var names -> STATE col names.
#'
#' @return list with elements:
#'   \describe{
#'     \item{X}{Corrected ensemble matrix (same dim as input X).}
#'     \item{weights_entry}{Named-list of learner weights per variable for this time, or NULL.}
#'     \item{weights_df_rows}{data.frame of (time,var,learner,weight) rows to rbind, possibly 0-row.}
#'     \item{diag}{list(comp = comparison_df, rmse = rmse_tbl) for this time.}
#'     \item{rmse_rows}{data.frame(time,var,rmse_pre,rmse_post) to rbind, possibly 0-row.}
#'   }
#'
#' @keywords internal
#' @importFrom stats complete.cases
#' @noRd
sda_apply_debias_step <- function(
    t, obs.t, X, raw_prev, raw_mean_t,
    site_index, col_vars,
    obs.times, obs.mean,
    covariates_df, py, train_buf,
    name_map = debias_name_map
) {
  # Guard: need covariates and previous time
  if (t <= 1 || is.null(covariates_df)) {
    return(list(
      X = X,
      weights_entry  = NULL,
      weights_df_rows = utils::head(data.frame(time=character(),var=character(),learner=character(),weight=numeric()), 0),
      diag = list(comp = debias_build_comp_df(site_index, col_vars, raw_mean_t, raw_mean_t, rep(NA_real_, length(col_vars))),
                  rmse = data.frame(var=unique(col_vars), rmse_pre=NA_real_, rmse_post=NA_real_)),
      rmse_rows = utils::head(data.frame(time=character(),var=character(),rmse_pre=numeric(),rmse_post=numeric()), 0)
    ))
  }
  
  # Build obs and covariate matrices
  obs_prev_vec <- debias_obs_vec_for_time(t - 1, site_index, col_vars, obs.mean, name_map)
  cov_prev_mat <- debias_cov_by_columns(covariates_df, obs.times[t - 1], site_index)
  cov_t_mat    <- debias_cov_by_columns(covariates_df, obs.times[t],     site_index)
  
  pred_resid <- numeric(ncol(X))
  vars <- unique(col_vars)
  weights_entry <- list()
  weights_df_rows <- utils::head(data.frame(time=character(),var=character(),learner=character(),weight=numeric(), stringsAsFactors = FALSE), 0)
  
  add_weight_rows <- function(time_label, var, w_named) {
    if (is.null(names(w_named))) names(w_named) <- paste0("learner_", seq_along(w_named))
    data.frame(
      time    = rep(time_label, length(w_named)),
      var     = rep(var,        length(w_named)),
      learner = names(w_named),
      weight  = as.numeric(w_named),
      stringsAsFactors = FALSE
    )
  }
  
  for (v in vars) {
    cols_v    <- which(col_vars == v)
    y_v_all   <- obs_prev_vec[cols_v] - as.numeric(raw_prev[cols_v])
    Xprev_all <- cbind(cov_prev_mat[cols_v, , drop = FALSE],
                       raw = as.numeric(raw_prev[cols_v]))
    mask <- !is.na(y_v_all) & stats::complete.cases(Xprev_all)
    if (any(mask)) {
      rec <- if (exists(v, train_buf, inherits = FALSE)) get(v, train_buf) else list(X = NULL, y = NULL)
      rec$X <- rbind(rec$X, Xprev_all[mask, , drop = FALSE])
      rec$y <- c(rec$y,  y_v_all[mask])
      assign(v, rec, train_buf)
      
      py$train_full_model(name = as.character(v),
                          X = as.matrix(rec$X),
                          y = as.numeric(rec$y))
      
      w_now <- try(py$get_model_weights(as.character(v)), silent = TRUE)
      if (!inherits(w_now, "try-error") && !is.null(w_now) && is.finite(w_now)) {
        w_now <- as.numeric(w_now)
        if (w_now < 0) w_now <- 0
        if (w_now > 1) w_now <- 1
        w_named <- c(KNN = w_now, TREE = 1 - w_now)
        weights_entry[[as.character(v)]] <- w_named
        weights_df_rows <- rbind(weights_df_rows, add_weight_rows(obs.t, as.character(v), w_named))
      }
    }
    
    if (py$has_model(as.character(v))) {
      Xt_v <- cbind(cov_t_mat[cols_v, , drop = FALSE],
                    raw = as.numeric(raw_mean_t[cols_v]))
      ok <- stats::complete.cases(Xt_v)
      if (any(ok)) {
        preds <- py$predict_residual(as.character(v), Xt_v[ok, , drop = FALSE])
        pred_resid[cols_v[ok]] <- as.numeric(preds)
      }
    }
  }
  
  pred_resid[!is.finite(pred_resid)] <- 0
  
  pre_mean  <- raw_mean_t
  post_mean <- raw_mean_t + pred_resid
  obs_t_vec <- debias_obs_vec_for_time(t, site_index, col_vars, obs.mean, name_map)
  
  comp_df  <- debias_build_comp_df(site_index, col_vars, pre_mean, post_mean, obs_t_vec)
  rmse_tbl <- debias_rmse_by_var(comp_df)
  rmse_tbl$time <- obs.t
  rmse_rows <- rmse_tbl[, c("time","var","rmse_pre","rmse_post")]
  
  # apply correction to the whole ensemble via mean-shift
  offsets   <- sweep(X, 2, raw_mean_t, FUN = "-")
  corrected <- post_mean
  X_new     <- sweep(offsets, 2, corrected, FUN = "+")
  
  list(
    X = X_new,
    weights_entry  = if (length(weights_entry)) weights_entry else NULL,
    weights_df_rows = weights_df_rows,
    diag = list(comp = comp_df, rmse = rmse_tbl),
    rmse_rows = rmse_rows
  )
}
