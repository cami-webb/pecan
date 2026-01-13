#!/usr/bin/env Rscript

# This script converts the BISm crop coefficient table from CSV into the
# packaged dataset `bism_kc_by_crop`.

raw_csv <- file.path("inst", "extdata", "bism_crop_coefficients.csv")

bism_kc_by_crop <- utils::read.csv(
    raw_csv,
    stringsAsFactors = FALSE,
    check.names = FALSE
)

usethis::use_data(bism_kc_by_crop, overwrite = TRUE)
