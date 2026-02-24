#!/usr/bin/env Rscript

options(warn = 1)

log_msg <- function(...) {
  cat(sprintf("[%s] ", Sys.time()), sprintf(...), "\n")
  flush.console()
}

log_msg("PEPRMT R runner starting")

# -------------------------
# Parse arguments from PEcAn
# -------------------------


# -------------------------
# Install / Load peprmt
# -------------------------
if (!requireNamespace("peprmt", quietly = TRUE)) {
  log_msg("Installing peprmt from GitHub")
  remotes::install_github("https://github.com/abbylewis/PEPRMT-Tidal")
}

library(peprmt)

log_msg("PEPRMT loaded successfully from GitHub")

# -------------------------
# READ INPUTS
# -------------------------
log_msg("Reading inputs")

# -------------------------
# RUN MODEL
# -------------------------
log_msg("Running PEPRMT model")

result <- tryCatch({
  
  ### ADD MODEL RUN HERE ###
  
  # Temporary stub so script runs:
  Sys.sleep(1)
  list(status = "ok")
  
}, error = function(e) {
  log_msg("MODEL ERROR: %s", e$message)
  quit(status = 1)
})

# -------------------------
# WRITE OUTPUTS
# -------------------------
log_msg("Writing outputs")

# Replace with real output logic
out_file <- file.path(output_dir, "peprmt_output.txt")
writeLines("Model completed successfully", out_file)

log_msg("Output written to %s", out_file)

quit(status = 0)