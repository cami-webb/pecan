
# R/run_sobol_analysis.R

#' Run Sobol Sensitivity Analysis
#' @param settings a PEcAn Settings or MultiSettings object
#' @export
#' @return sobol analysis result
#' 



sobol_analysis <- function( settings ) {
  
  ensemble_size = settings$ensemble$size
  sobol_obj <- PEcAn.uncertainty::generate_joint_ensemble_design(settings = settings, ensemble_size = ensemble_size, sobol = TRUE)
  input_design <- sobol_obj$X
  ensemble_size <- nrow(input_design)
  
  #check to see if there are posterior.files tags under pft
  posterior.files <-   settings$pfts %>%
    purrr::map_chr("posterior.files", .default = NA_character_)
  PEcAn.workflow::run.write.configs(settings = settings,
                                    ensemble.size = ensemble_size,
                                    write = isTRUE(settings$database$bety$write), # treat null as FALSE
                                    posterior.files = posterior.files,
                                    overwrite = TRUE ,
                                    input_design = input_design)
  
  #running the model 
  PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = stop_on_error)
  
 

 sobol_obj <- PEcAn.uncertainty::compute_sobol_indices(outdir = settings$outdir, 
                                   sobol_obj = sobol_obj, 
                                   var = "GPP") 
 
 return(sobol_obj) 
}

