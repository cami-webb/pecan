#' ---
#' title: "Statewide irrigation workflow"
#' author: "Alexey N. Shiklomanov"
#' ---

Sys.setenv(
  "TAR_PROJECT" = "all",
  "OMP_NUM_THREADS" = 1
)

library(targets)

# devtools::document("modules/data.land")
# devtools::install("modules/data.land", upgrade = FALSE, reload = TRUE)

#' Run the pipeline. Targets that are already up-to-date will be skipped.
tar_make()

if (interactive()) {
  tar_load_everything()
}

# tar_invalidate(dp_with_crops)
# tar_load("phenology_crops")
# tar_load(c("design_points", "dp_with_crops", "phenology"))
