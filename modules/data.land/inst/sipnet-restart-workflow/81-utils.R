#' settings for whole run with many paths per input ->
#' settings for one ensemble member with one path per input
#'
#' @param settings single-site settings object (not Multisettings)
#' @param inputs named list of input indices (one row of the sample design)
subset_paths <- function(settings, path_nums) {
  for (input in names(path_nums)) {
    if (!(input %in% names(settings$run$inputs))) {
      next
    }
    path_idx <- path_nums[[input]]
    all_paths <- settings$run$inputs[[input]]$path
    if (path_idx > length(all_paths)) {
      PEcAn.logger::logger.severe("No path at input ", sQuote(input), " index ", path_idx)
    }
    settings$run$inputs[[input]]$path <- all_paths[[path_idx]]
  }
  settings
}

crop2pft <- function(crop_code) {
  # crop_code <- c("F1", "R1", "G2", "F16")
  cls <- substr(crop_code, 1, 1)
  dplyr::case_when(
    cls == "D" ~ "temperate.deciduous",
    cls == "F" ~ "annual_crop",
    cls == "G" ~ "grass",
    cls == "P" ~ "grass",
    cls == "R" ~ "grass",
    is.na(crop_code) ~ "soil",
    TRUE ~ "UNKNOWN_PFT"
  )
}
