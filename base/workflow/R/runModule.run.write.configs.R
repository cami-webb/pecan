#' Generate model-specific run configuration files for one or more PEcAn runs
#'
#' @param settings a PEcAn Settings or MultiSettings object
#' @param overwrite logical: Replace config files if they already exist?
#' @param input_design Optional. Input design specification. Can be:
#'   \itemize{
#'     \item A list with \code{ensemble} and/or \code{sensitivity} entries
#'     \item A single data.frame (interpreted as ensemble design)
#'     \item NULL to auto-generate designs based on settings
#'   }
#'
#' @return A modified settings object, invisibly
#'
#' @details
#' This function serves as the orchestration layer between PEcAn workflows and
#' the config-writing machinery. It generates appropriate input designs
#' (ensemble and/or SA) if not provided. For MultiSettings, it generates designs once
#' from the first site then shares across all sites for consistent sampling. Finally,
#' it delegates to \code{\link{run.write.configs}} for actual config generation.
#' The input design determines how parameter samples and input files (met, soil,
#' etc.) are coordinated across runs. Ensemble designs typically use random or
#' quasi-random sampling, while SA designs hold non-parameter inputs constant
#' (OAT methodology).
#'
#' @importFrom dplyr %>%
#' @importFrom rlang %||%
#' @export


runModule.run.write.configs <- function(settings,
                                        overwrite = TRUE,
                                        input_design = NULL) {

  if (PEcAn.settings::is.MultiSettings(settings)) {
    if (overwrite && file.exists(file.path(settings$rundir, "runs.txt"))) {
      PEcAn.logger::logger.warn("Existing runs.txt file will be removed.")
      unlink(file.path(settings$rundir, "runs.txt"))
    }
    
    # prepare designs once for all sites (consistent sampling)
    designs <- .prepare_input_designs(settings[1], input_design)
    
    return(PEcAn.settings::papply(settings,
                                  runModule.run.write.configs,
                                  overwrite = FALSE,
                                  input_design = designs))

  } else if (PEcAn.settings::is.Settings(settings)) {
    if (is.null(settings$ensemble$samplingspace$parameters$method)) {
      settings$ensemble$samplingspace$parameters$method <- "uniform"
    }
    
    # prepare designs (may already be normalized from MultiSettings)
    designs <- .prepare_input_designs(settings, input_design)

    # determine ensemble size from design
    ensemble_size <- if (!is.null(designs$ensemble)) {
      nrow(designs$ensemble)
    } else {
      settings$ensemble$size %||% 1
    }

    # check to see if there are posterior.files tags under pft
    posterior.files <- settings$pfts %>%
      purrr::map_chr("posterior.files", .default = NA_character_)

    return(PEcAn.workflow::run.write.configs(
      settings = settings,
      ensemble.size = ensemble_size,
      write = isTRUE(settings$database$bety$write), # treat null as FALSE
      posterior.files = posterior.files,
      overwrite = overwrite,
      input_design = designs
    ))
  } else {
    stop("runModule.run.write.configs only works with Settings or MultiSettings")
  }
}


#' Prepare input designs for ensemble and sensitivity analysis
#'
#' Normalizes and generates input design matrices. This helper ensures
#' consistent handling of the various input_design formats and
#' auto-generates designs when needed.
#'
#' @param settings A single PEcAn settings object
#' @param input_design Input design specification (see \code{runModule.run.write.configs})
#' @return A list with \code{ensemble} and \code{sensitivity} entries (each a data.frame or NULL)
#'
#' @details
#' Input normalization rules:
#' \itemize{
#'   \item If \code{input_design} is already a list with \code{ensemble}/\code{sensitivity}
#'         keys, return as-is
#'   \item If \code{input_design} is a single data.frame, interpret as ensemble design
#'   \item If NULL and \code{settings$ensemble} exists, generate via
#'         \code{generate_joint_ensemble_design}
#'   \item If NULL and \code{settings$sensitivity.analysis} exists, generate via
#'         \code{generate_OAT_SA_design}
#' }
#'
#' @keywords internal

.prepare_input_designs <- function(settings, input_design) {

  # already normalized? return as-is
  if (is.list(input_design) &&
      any(c("ensemble", "sensitivity") %in% names(input_design))) {
    return(input_design)
  }

  designs <- list(ensemble = NULL, sensitivity = NULL)

  # single data.frame = ensemble design
 if (is.data.frame(input_design)) {
    designs$ensemble <- input_design
  }

  # generate ensemble design if needed
  if (is.null(designs$ensemble) && "ensemble" %in% names(settings)) {
    ensemble_size <- settings$ensemble$size %||% 1
    design_result <- PEcAn.uncertainty::generate_joint_ensemble_design(
      settings = settings,
      ensemble_size = ensemble_size
    )
    designs$ensemble <- design_result$X
  }

  # generate SA design if needed
  if (is.null(designs$sensitivity) && "sensitivity.analysis" %in% names(settings)) {
    design_result <- PEcAn.uncertainty::generate_OAT_SA_design(settings)
    designs$sensitivity <- design_result$X
  }

  return(designs)
}