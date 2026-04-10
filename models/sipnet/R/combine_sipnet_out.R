#!/usr/bin/env Rscript

#' Combine a bunch of `sipnet.out` files into a single file
#'
#' @param directory Parent directory to search for `sipnet.out` files. You must
#' provide either this or an explicit vector of `files`.
#' @param outfile File to which combined `sipnet.out` will be written
#' @param files (optional) Explicit vector of paths to `sipnet.out` files to
#' combine. Note that files need not be named `sipnet.out`, but they must be
#' readable with [read_sipnet_out()].
#'
#' @return `outfile` (path to output file), invisibly
#' @export
combine_sipnet_out <- function(directory, outfile, files = NULL) {
  if (missing(directory) && is.null(files)) {
    PEcAn.logger::logger.severe("Must provide either `directory` or `files`")
  }
  if (is.null(files)) {
    files <- sort(list.files(directory, "sipnet\\.out", full.names = TRUE, recursive = TRUE))
  }
  if (!(length(files) > 0)) {
    PEcAn.logger::logger.severe("No files provided; nothing to combine.")
  }
  flist <- lapply(files, read_sipnet_out)
  combined <- do.call(rbind.data.frame, flist)
  # Mimic the SIPNET fixed-width right-aligned format
  combined_fwf <- format(combined, justify = "right")
  write.table(combined_fwf, outfile, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "  ")
  invisible(outfile)
}
