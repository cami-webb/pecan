#' Convert fractional NDTI drop to SIPNET tillage effectiveness
#'
#' Maps the fractional NDTI drop produced by the CCMMF NDTI pipeline
#' to the \code{tillage_eff_0to1} value used in the PEcAn events JSON
#' schema and written to SIPNET \code{events.in} by
#' \code{\link[PEcAn.SIPNET]{write.events.SIPNET}}.
#'
#' @param dates Date vector. Observation dates corresponding to each
#'   tillage event. Same length as \code{delta_ndti}.
#' @param delta_ndti numeric vector. Fractional NDTI drop in \[0, 1\],
#'   computed as \code{(max_before_min - min_val) / max_before_min}
#'   from a smoothed NDTI time series over the fallow season.
#' @param no_till_threshold numeric scalar. \code{delta_ndti} values at
#'   or below this threshold are mapped to zero effectiveness (no-till).
#'   Default 0.30, based on Dietze et al. (pers. comm.).
#' @param conv_till_threshold numeric scalar. \code{delta_ndti} values
#'   at or above this threshold are mapped to \code{max_modifier}
#'   (conventional tillage). Default 0.70.
#' @param max_modifier numeric scalar. Maximum \code{tillage_eff_0to1}
#'   value, corresponding to conventional tillage. Default 1.0. Can be
#'   lowered to constrain the modeled tillage response without changing
#'   the shape of the mapping. Useful for sensitivity analysis or when
#'   field calibration data suggest a lower ceiling.
#' @param method character. Functional form of the NDTI-to-effectiveness
#'   mapping between the two thresholds:
#'   \describe{
#'     \item{\code{"linear"}}{(default) Effectiveness scales linearly
#'       with NDTI drop between the two thresholds.}
#'     \item{\code{"saturating"}}{Effectiveness follows
#'       \code{1 - exp(-3 * x)} between the thresholds, reaching
#'       approximately 95\% of \code{max_modifier} at
#'       \code{conv_till_threshold}. Appropriate if moderate tillage
#'       already produces a near-maximum decomposition response.}
#'   }
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{date}{Date. Same as input \code{dates}.}
#'     \item{tillage_eff_0to1}{numeric. Tillage effectiveness ready for
#'       use as \code{tillage_eff_0to1} in a PEcAn events JSON file.}
#'   }
#'
#' @details
#' ## Background
#'
#' The NDTI (Normalized Difference Tillage Index) drops sharply after a
#' tillage event as crop residue is incorporated into the soil. The
#' fractional drop between the pre-tillage maximum and the post-tillage
#' minimum over the fallow season serves as a proxy for tillage intensity.
#'
#' ## Default thresholds
#'
#' Based on empirical analysis of California cropland NDTI time series
#' (Dietze & Kanee, pers. comm.):
#'
#' \tabular{ll}{
#'   \strong{NDTI drop}    \tab \strong{Tillage category} \cr
#'   below 0.30            \tab no-till                   \cr
#'   0.30 -- 0.70          \tab conservation / reduced    \cr
#'   above 0.70            \tab conventional              \cr
#' }
#'
#' The linear default is the most parsimonious assumption given that no
#' empirical calibration data are yet available. The \code{method}
#' argument and the exposed threshold parameters allow this to be
#' updated without changing function signatures once calibration data
#' become available (e.g. from SOC or soil respiration measurements).
#'
#' ## NA handling
#'
#' \code{NA} values in \code{delta_ndti} propagate to
#' \code{tillage_eff_0to1} without error.
#'
#' @references
#' Daughtry, C.S.T., Hunt, E.R., Doraiswamy, P.C., McMurtrey, J.E.
#' (2005). Remote sensing the spatial distribution of crop residues.
#' \emph{Agronomy Journal}, 97(3), 864--871.
#' \doi{10.2134/agronj2004.0291}
#'
#' @examples
#' dates  <- as.Date(c("2022-03-01", "2022-11-01"))
#' drops  <- c(0.25, 0.80)
#'
#' # Default: linear mapping, thresholds from Dietze & Kanee
#' ndti_to_sipnet_tillage(dates, drops)
#'
#' # Saturating response
#' ndti_to_sipnet_tillage(dates, drops, method = "saturating")
#'
#' # Custom thresholds (site-specific calibration)
#' ndti_to_sipnet_tillage(dates, drops,
#'   no_till_threshold   = 0.20,
#'   conv_till_threshold = 0.60
#' )
#'
#' # Cap maximum response at 0.5 for sensitivity analysis
#' ndti_to_sipnet_tillage(dates, drops, max_modifier = 0.5)
#'
#' @export
ndti_to_sipnet_tillage <- function(
    dates,
    delta_ndti,
    no_till_threshold   = 0.30,
    conv_till_threshold = 0.70,
    max_modifier        = 1.0,
    method              = c("linear", "saturating")
) {
  method <- match.arg(method)

  ## --- input checks ---
  if (length(dates) != length(delta_ndti)) {
    PEcAn.logger::logger.severe(
      "dates and delta_ndti must be the same length; got ",
      length(dates), " and ", length(delta_ndti), "."
    )
  }

  if (!is.numeric(no_till_threshold) ||
      length(no_till_threshold) != 1 ||
      !is.numeric(conv_till_threshold) ||
      length(conv_till_threshold) != 1) {
    PEcAn.logger::logger.severe(
      "no_till_threshold and conv_till_threshold must each be a single numeric value."
    )
  }

  if (no_till_threshold >= conv_till_threshold) {
    PEcAn.logger::logger.severe(
      "no_till_threshold (", no_till_threshold, ") must be less than ",
      "conv_till_threshold (", conv_till_threshold, ")."
    )
  }

  if (!is.numeric(max_modifier) || length(max_modifier) != 1 || max_modifier < 0) {
    PEcAn.logger::logger.severe(
      "max_modifier must be a single non-negative numeric value; got ",
      max_modifier, "."
    )
  }

  dates      <- as.Date(dates)
  delta_ndti <- as.numeric(delta_ndti)

  if (any(delta_ndti < 0, na.rm = TRUE)) {
    PEcAn.logger::logger.warn(
      "Negative delta_ndti values detected; these will be treated as no-till (clamped to 0)."
    )
  }

  ## --- mapping ---
  # Scale delta_ndti to [0, 1] within the threshold window.
  # Values below no_till_threshold → 0; above conv_till_threshold → 1.
  scaled <- (delta_ndti - no_till_threshold) /
    (conv_till_threshold - no_till_threshold)
  scaled <- pmin(pmax(scaled, 0), 1)

  if (method == "linear") {
    tillage_eff <- scaled * max_modifier
  } else {
    # saturating: 1 - exp(-3x) reaches ~95% of max at x = 1
    tillage_eff <- max_modifier * (1 - exp(-3 * scaled))
  }

  # preserve NAs from input — don't silently replace with zero
  tillage_eff[is.na(delta_ndti)] <- NA_real_

  data.frame(
    date             = dates,
    tillage_eff_0to1 = tillage_eff
  )
} # ndti_to_sipnet_tillage