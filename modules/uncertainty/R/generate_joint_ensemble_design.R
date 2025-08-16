# This function generates a single joint ensemble design matrix (input indices)
# which is reused across all sites. This ensures consistent sampling of inputs
# (e.g., meteorology, parameters) to preserve spatial correlation and enable 
# meaningful multi-site Sobol sensitivity analysis.

generate_joint_ensemble_design <- function(settings, ensemble_size ) {
  ens.sample.method <- settings$ensemble$samplingspace$parameters$method
  design_matrix <- data.frame()
  sampled_inputs <- list()
  posterior.files = rep(NA, length(settings$pfts))
  samp <- settings$ensemble$samplingspace
  parents <- lapply(samp, '[[', 'parent')
  order <- names(samp)[lapply(parents, function(tr) which(names(samp) %in% tr)) %>% unlist()]
  samp.ordered <- samp[c(order, names(samp)[!(names(samp) %in% order)])]

  for (i in seq_along(samp.ordered)) {
    input_tag <- names(samp.ordered)[i]
    parent_name <- samp.ordered[[i]]$parent

    parent_ids <- if (!is.null(parent_name)) sampled_inputs[[parent_name]] else NULL

    input_result <- PEcAn.uncertainty::input.ens.gen(
      settings = settings,
      input = input_tag,
      method = samp.ordered[[i]]$method,
      parent_ids = parent_ids
    )

    sampled_inputs[[input_tag]] <- input_result$ids
    design_matrix[[input_tag]] <- input_result$ids
  }

  # Sample parameters
  PEcAn.uncertainty::get.parameter.samples(settings, posterior.files, ens.sample.method)

  # Load samples from file
  samples.file <- file.path(settings$outdir, "samples.Rdata")
  samples <- new.env()
  if (file.exists(samples.file)) {
    load(samples.file, envir = samples)
    if (!is.null(samples$ensemble.samples)) {
      # Just a placeholder: extract representative trait index per ensemble member
      # You may want to flatten or select indices per trait
      design_matrix[["param"]] <- seq_len(ensemble_size)
    } else {
      PEcAn.logger::logger.warn("ensemble.samples not found in samples.Rdata")
    }
  } else {
    PEcAn.logger::logger.error(samples.file, "not found, this file is required")
  }

  return(design_matrix)
}
