#' process model output and return the results as a list
#'
#' @return an list of model outputs
#' @export


processModelOutput <- function(){

runs_file <- file.path(settings$outdir, "runs.txt")

if (file.exists(runs_file)) {
  run_ids <- readLines(runs_file) 
} else {
  stop("runs.txt not found - check settings$outdir")
}

# Loop to read outputs for each run
all_model_out <- list()
for (i in run_ids) { 
  run_specific_outdir <- file.path(settings$outdir, i)  
  
  # Read output 
  model_out <- read.output(runid = i, 
                           outdir = run_specific_outdir)
  
  all_model_out[[i]] <- model_out
}

}