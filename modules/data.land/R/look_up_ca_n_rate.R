#' Look up California N application rates by crop
#'
#' Returns recommended nitrogen application rate ranges for California crops,
#' based on CDFA-FREP guidelines and UC ANR publications. Rates are provided
#' in both imperial (lbs N/acre) and SI (g N/m2) units.
#'
#' Matching is case-insensitive. Exact matches are returned directly.
#' If no exact match is found, partial matching is used to suggest
#' possible crops and an empty data frame is returned.
#'
#' @param crop Character string. Crop name to look up.
#' @param pft_group Optional character string. Filter results to a specific
#'   plant functional type group (e.g. "row", "woody", "rice").
#' @param unit Character, one of "g_m2" (default) or "lbs_acre". Controls
#'   which columns appear as `min_n` and `max_n` in the output.
#'
#' @return A data frame with columns: `pft_group`, `crop`, `min_n`, `max_n`,
#'   `source`. The `min_n` and `max_n` columns are in the requested unit.
#'   Returns an empty data frame (with a warning) if no match is found.
#'
#' @seealso [look_up_fertilizer_components()] for fertilizer nutrient
#'   composition (N/C fractions) from the SWAT/DayCent database.
#'   [ca_n_application_rate] for the underlying dataset.
#'
#' @examples
#' look_up_ca_n_rate("Tomatoes, Processing")
#' look_up_ca_n_rate("corn")
#' look_up_ca_n_rate("wheat", unit = "lbs_acre")
#' look_up_ca_n_rate("pistachio", pft_group = "woody")
#'
#' @export
look_up_ca_n_rate <- function(
    crop,
    pft_group = NULL,
    unit = c("g_m2", "lbs_acre")
) {
  unit <- match.arg(unit)

  dat <- PEcAn.data.land::ca_n_application_rate

  if (!is.null(pft_group)) {
    dat <- dat |>
      dplyr::filter(tolower(.data$pft_group) == tolower(.env$pft_group))
  }

  # try exact match first (case-insensitive)
  result <- dat |>
    dplyr::filter(tolower(.data$crop) == tolower(.env$crop))

  # if no exact match, try partial and suggest
  if (nrow(result) == 0) {
    partial <- dat |>
      dplyr::filter(grepl(tolower(.env$crop), tolower(.data$crop), fixed = TRUE))

    if (nrow(partial) > 0) {
      PEcAn.logger::logger.warn(
        "No exact match for '", crop, "'. ",
        "Did you mean one of: ",
        paste(unique(partial$crop), collapse = ", "), "?"
      )
    } else {
      PEcAn.logger::logger.warn(
        "No N application rate found for crop '", crop, "'"
      )
    }
    return(data.frame(
      pft_group = character(),
      crop = character(),
      min_n = numeric(),
      max_n = numeric(),
      source = character(),
      stringsAsFactors = FALSE
    ))
  }

  if (unit == "g_m2") {
    result |>
      dplyr::transmute(
        .data$pft_group,
        .data$crop,
        min_n = .data$min_n_g_m2,
        max_n = .data$max_n_g_m2,
        .data$source
      )
  } else {
    result |>
      dplyr::transmute(
        .data$pft_group,
        .data$crop,
        min_n = .data$min_n_lbs_acre,
        max_n = .data$max_n_lbs_acre,
        .data$source
      )
  }
}


#' Look up California compost amendment properties
#'
#' Returns properties of organic amendment materials including carbon and
#' nitrogen content, C:N ratio, and plant-available nitrogen (PAN).
#' Data are specific to California agriculture, sourced from NRCS Practice
#' Standard 808 and CDFA Healthy Soils Program guidelines.
#'
#' Matching is case-insensitive. Exact matches are returned directly.
#' If no exact match is found, partial matching suggests possible materials.
#'
#' @param material Character string. Amendment material to look up.
#' @param n_class Optional, one of "LOWER" or "HIGHER". Filter by N class.
#'
#' @return A data frame with columns: `material`, `cn_avg`, `c_pct`, `n_pct`,
#'   `pan_pct`, `n_class`, `total_c_min_g_m2`, `total_c_max_g_m2`,
#'   `total_n_min_g_m2`, `total_n_max_g_m2`.
#'   Returns an empty data frame (with a warning) if no match is found.
#'
#' @seealso [look_up_fertilizer_components()] for fertilizer nutrient
#'   composition (N/C fractions) from the SWAT/DayCent database.
#'   [ca_compost_amendment] for the underlying dataset.
#'
#' @examples
#' look_up_ca_compost_amendment("Cow manure")
#' look_up_ca_compost_amendment("Poultry litter", n_class = "HIGHER")
#'
#' @export
look_up_ca_compost_amendment <- function(material, n_class = NULL) {
  dat <- PEcAn.data.land::ca_compost_amendment

  if (!is.null(n_class)) {
    dat <- dat |>
      dplyr::filter(toupper(.data$n_class) == toupper(.env$n_class))
  }

  # try exact match first (case-insensitive)
  result <- dat |>
    dplyr::filter(tolower(.data$material) == tolower(.env$material))

  # if no exact match, try partial and suggest
  if (nrow(result) == 0) {
    partial <- dat |>
      dplyr::filter(grepl(tolower(.env$material), tolower(.data$material), fixed = TRUE))

    if (nrow(partial) > 0) {
      PEcAn.logger::logger.warn(
        "No exact match for '", material, "'. ",
        "Did you mean one of: ",
        paste(unique(partial$material), collapse = ", "), "?"
      )
    } else {
      PEcAn.logger::logger.warn(
        "No compost amendment found for material '", material, "'"
      )
    }
    return(data.frame(
      material = character(), cn_avg = numeric(), c_pct = numeric(),
      n_pct = numeric(), pan_pct = numeric(), n_class = character(),
      total_c_min_g_m2 = numeric(), total_c_max_g_m2 = numeric(),
      total_n_min_g_m2 = numeric(), total_n_max_g_m2 = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  result |>
    dplyr::select(
      "material", "cn_avg", "c_pct", "n_pct", "pan_pct", "n_class",
      "total_c_min_g_m2", "total_c_max_g_m2",
      "total_n_min_g_m2", "total_n_max_g_m2"
    )
}
