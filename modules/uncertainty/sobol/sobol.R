#loading libs
library("PEcAn.all")
library(PEcAn.settings)
library(PEcAn.workflow)
library(PEcAn.logger)
library(PEcAn.utils)
library(PEcAn.remote)
library(PEcAn.uncertainty)
library(dplyr)
library(ggplot2)
library(data.table)
library(assertthat)
library(lubridate)
library(sensitivity)
library(PEcAn.SIPNET)

#reading XML 

args <- list(continue = FALSE)

settings <- PEcAn.settings::read.settings("/projectnb/dietzelab/bthomas/pecan_runs/sipnet_test/pecan_updated.xml")
settings<-settings[1]

if ("benchmarking" %in% names(settings)) {
  library(PEcAn.benchmark)
  settings <- papply(settings, read_settings_BRR)
}

if ("sitegroup" %in% names(settings)) {
  if (is.null(settings$sitegroup$nSite)) {
    settings <- PEcAn.settings::createSitegroupMultiSettings(settings,
                                                             sitegroupId = settings$sitegroup$id
    )
  } else {
    settings <- PEcAn.settings::createSitegroupMultiSettings(
      settings,
      sitegroupId = settings$sitegroup$id,
      nSite = settings$sitegroup$nSite
    )
  }
  # zero out so don't expand a second time if re-reading
  settings$sitegroup <- NULL
}

# Update/fix/check settings.
# Will only run the first time it's called, unless force=TRUE
settings <-
  PEcAn.settings::prepare.settings(settings, force = FALSE)

# Write pecan.CHECKED.xml
PEcAn.settings::write.settings(settings, outputfile = "pecan.CHECKED.xml")
# start from scratch if no continue is passed in
status_file <- file.path(settings$outdir, "STATUS")
if (args$continue && file.exists(status_file)) {
  file.remove(status_file)
}



#conducting the sampling 



ensemble_size <- settings$ensemble$size
input_design <- PEcAn.uncertainty::generate_joint_ensemble_design(settings=settings,ensemble_size=ensemble_size)
input_design



#convverting input indices to input



# Sample inputs 
ipsamples <- list()

input_tags <- names(settings$run$inputs)
input_tags
for (input_tag in input_tags) {
  if (input_tag %in% colnames(input_design)) {
    input_paths <- settings$run$inputs[[input_tag]]$path  # List of all possible paths
    if (!is.list(input_paths) || length(input_paths) < max(input_design[[input_tag]])) {
      stop(paste("Not enough paths for", input_tag, "- max index:", max(input_design[[input_tag]]), "but only", length(input_paths), "available"))
    }
    input_indices <- input_design[[input_tag]]
    ipsamples[[input_tag]] <- list(
      ipsamples = lapply(input_indices, function(idx) input_paths[[idx]])  # Select paths by index
    )
  } else {
    message(paste("No column for", input_tag, "in input_design - skipping sampling"))
  }
}
#input file path to inputs




# extracting parameter 


samples.file <- file.path(settings$outdir, "samples.Rdata")
if (file.exists(samples.file)) {
  samples <- new.env()
  load(samples.file, envir = samples) ## loads ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples
  trait.samples <- samples$trait.samples
  
  
  trait_sample_indices <- input_design[["param"]]
  ensemble.samples <- list()
  for (pft in names(trait.samples)) {
    pft_traits <- trait.samples[[pft]]
    ensemble.samples[[pft]] <- as.data.frame(
      lapply(
        names(pft_traits),
        function(trait) pft_traits[[trait]][trait_sample_indices]
      )
    )
    names(ensemble.samples[[pft]]) <- names(pft_traits)
  }
  sa.samples <- samples$sa.samples
  runs.samples <- samples$runs.samples
  ## env.samples <- samples$env.samples
  
} else {
  PEcAn.logger::logger.error(samples.file, "not found, this file is required by the run.write.configs function")
}

ensemble.samples
all_params <- ensemble.samples$temperate.deciduous.HPDA



#creating sobol object





#param_count <- ncol(all_params)  # Number of parameters (e.g., 13)
#param_count
#N_small <- ceiling(50 / (param_count + 2))  # ~2 for 50 rows; adjust as needed
#X1_small <- all_params[1:(N_small), ]  # First N rows
#X2_small <- all_params[(N_small + 1):(2 * N_small), ]  # Next N rows
#sobol_obj <- soboljansen(model = NULL, X1 = X1_small, X2 = X2_small)
#U <- sobol_obj$X
#length(U)




X1 <- all_params[1:25, ]
X2 <- all_params[26:50, ]
sobol_obj <- soboljansen(model = NULL, X1 = X1, X2 = X2)
U <- sobol_obj$X
ensemble.samples$temperate.deciduous.HPDA <-U
all.param.samples <- list(
  trait.samples = trait.samples,
  ensemble.samples = ensemble.samples ,
  sa.samples = sa.samples,
  runs.samples = runs.samples
  # env.samples = samples$env.samples  # Uncomment if needed
)


#running the site configs 

#check to see if there are posterior.files tags under pft
posterior.files <-   settings$pfts %>%
  purrr::map_chr("posterior.files", .default = NA_character_)

PEcAn.workflow::run.write.configs(
                                  settings = settings,
                                  write = isTRUE(settings$database$bety$write), # treat null as FALSE
                                  posterior.files = posterior.files,
                                  overwrite = TRUE ,
                                  input_design = input_design,
                                  all.param.samples = all.param.samples 
                                 )




#running the model 
PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = stop_on_error)
 

#reading output 

runs_file <- file.path(settings$outdir, "runs.txt")
if (file.exists(runs_file)) {
  run_ids <- readLines(runs_file)  # Your 50 IDs
} else {
  stop("runs.txt not found - check settings$outdir")
}

# Loop to read outputs for each run
all_model_out <- list()
for (i in run_ids) {
  # Correct run-specific outdir 
  run_specific_outdir <- file.path(settings$outdir, i)  
  
  # Read output (add variables/start.year/end.year if needed)
  model_out <- read.output(runid = i, 
                           outdir = run_specific_outdir)
                           
  all_model_out[[i]] <- model_out
}










#conducting the sobol

y <- sapply(run_ids, function(rid) {
  out_list <- all_model_out[[rid]]
  mean(out_list$GPP, na.rm = TRUE) 
})

# Check lengths match
print(length(y))  # Should be 50
print(nrow(sobol_obj$X))  # Should be 50

# Compute indices
tell(sobol_obj, y)

# View/plot results
print(sobol_obj)
