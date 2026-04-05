#!/usr/bin/env Rscript

#' @export
split_inputs.SIPNET <- function(start.time, stop.time, inputs, overwrite = FALSE, outpath = NULL) {
  result <- inputs
  if ("met" %in% names(inputs)) {
    result[["met"]][["path"]] <- split_sipnet_met(
      start.time,
      stop.time,
      inputs$met$path,
      overwrite = overwrite,
      outpath = outpath
    )
  }

  if ("events" %in% names(inputs)) {
    result[["events"]][["path"]] <- split_sipnet_events(
      start.time,
      stop.time,
      inputs$events$path,
      overwrite = overwrite,
      outpath = outpath
    )
  }
  result
}

split_sipnet_events <- function(start.time, stop.time, eventfile, overwrite = FALSE, outpath = FALSE) {
  # Read events.in
  # eventfile <- sipnet_eventfile
  # start.time <- dstart
  # stop.time <- dend
  prefix <- sub(".in", "", basename(eventfile), fixed = TRUE)
  outpath <- outpath %||% dirname(eventfile)
  dir.create(outpath, recursive = TRUE, showWarnings = FALSE)

  #Changing the name of the files, so it would contain the name of the hour as well.
  formatted_start <- strftime(start.time)
  formatted_stop <- strftime(stop.time)
  outfile <- file.path(outpath, paste0(prefix, ".", formatted_start, "-", formatted_stop, ".in"))
  names(outfile) <- paste(start.time, "-", stop.time)

  if (file.exists(outfile) && !overwrite) {
    PEcAn.logger::logger.warn(
      outfile,
      " already exists and overwrite is FALSE, so keeping existing file."
    )
    return(outfile)
  }

  # We can't use `read.table` or similar here because events file rows have
  # different numbers of columns depending on event type. Instead, we just
  # filter based on the event date, which is always the first two columns (year
  # and doy).
  events_in_raw <- readLines(eventfile)
  events_in_list <- strsplit(events_in_raw, "[[:space:]]+")
  years_in <- vapply(events_in_list, \(x) as.integer(x[[1]]), integer(1))
  doys_in <- vapply(events_in_list, \(x) as.integer(x[[2]]), integer(1))
  # Not using sipnet2datetime here because it returns times with time zones,
  # which could cause subtle timezone-related bugs
  dates_in <- as.Date(sprintf("%d-01-01", years_in)) + doys_in
  idx_keep <- (dates_in >= start.time) & (dates_in <= stop.time)
  if (length(idx_keep) == 0) {
    PEcAn.logger::logger.warn("No events to keep, so `events.in` will be empty")
  }
  events_out_str <- events_in_raw[idx_keep]
  writeLines(events_out_str, outfile)
  invisible(outfile)
}

## split clim file into smaller time units to use in KF
##' @author Mike Dietze and Ann Raiho
##' 
##' @param start.time start date and time for each SDA ensemble
##' @param stop.time stop date and time for each SDA ensemble
##' @param inputs list of model inputs to use in write.configs.SIPNET
##' @param overwrite Default FALSE
##' @param outpath if specified, write output to a new directory. Default NULL writes back to the directory being read
##' @description Splits climate met for SIPNET
##' 
##' @return file split up climate file
##' @importFrom rlang .data
##' @importFrom dplyr %>%
##' @export
split_sipnet_met <- function(start.time, stop.time, met, overwrite = FALSE, outpath = NULL) {
  path <- dirname(met)
  prefix <- sub(".clim", "", basename(met), fixed = TRUE)
  if(is.null(outpath)){
    outpath <- path
  }
  if(!dir.exists(outpath)) dir.create(outpath, recursive = TRUE)
  

  file <- NA
  names(file) <- paste(start.time, "-", stop.time)
  
  #Changing the name of the files, so it would contain the name of the hour as well.
  formatted_start <- gsub(' ',"_", as.character(start.time))
  formatted_stop <- gsub(' ',"_", as.character(stop.time))
  file <- paste0(outpath, "/", prefix, ".", formatted_start, "-", formatted_stop, ".clim")
  
  if(file.exists(file) && !overwrite){
    PEcAn.logger::logger.warn(
      file,
      " already exists and overwrite is FALSE, so keeping existing file."
    )
    return(file)
  }

  input.dat <- utils::read.table(met, header = FALSE)


  if (ncol(input.dat) == 14) {
    # V1 format
    in_posix <- sipnet2datetime(input.dat$V2, input.dat$V3, input.dat$V4)
  } else if (ncol(input.dat) == 12) {
    in_posix <- sipnet2datetime(input.dat$V1, input.dat$V2, input.dat$V3)
  } else {
    PEcAn.logger::logger.error("Unknown clim format; can't split met files.")
    return(NA_character_)
  }

  dat <- input.dat[in_posix >= start.time & in_posix < stop.time, ]
  
  
  ###### Write Met to file
  utils::write.table(dat, file, row.names = FALSE, col.names = FALSE)

  ###### Output input path to inputs
  #settings$run$inputs$met$path <- file
  return(file)
} # split_inputs.SIPNET
