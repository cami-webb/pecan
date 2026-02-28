#' Load posterior distributions and/or Monte Carlo samples for a PFT
#'
#' Detects posterior type by file contents, not filenames.
#' Monte Carlo samples (\code{trait.mcmc}) take precedence over
#' distribution summaries (\code{post.distns} / \code{prior.distns}).
#' Falls back to legacy \code{outdir} + DB lookup when
#' \code{posterior.file} is \code{NA}, with a deprecation warning.
#'
#' @param posterior.file path to a posterior file or directory, or \code{NA}.
#' @param outdir Legacy PFT output directory (fallback).
#' @param posteriorid Posterior ID for db lookup.
#' @param con db connection
#' @param hostname host name for db file lookup.
#' @return list with \code{prior.distns}, \code{trait.mcmc}, \code{is.pda}.
#' @keywords internal
#' @author Siddhey Patil
load.posteriors <- function(posterior.file,
                            outdir = NULL,
                            posteriorid = NULL,
                            con = NULL,
                            hostname = NULL) {
  result <- list(
    prior.distns = NULL,
    trait.mcmc = NULL,
    is.pda = FALSE
  )

  if (!is.na(posterior.file)) {
    result <- load.posteriors.from.path(posterior.file)
  } else {
    PEcAn.logger::logger.warn(
      "Reading posterior from pft$outdir is deprecated. ",
      "Please specify posterior.files explicitly."
    )
    result <- load.posteriors.legacy(
      outdir = outdir,
      posteriorid = posteriorid,
      con = con,
      hostname = hostname
    )
  }

  return(result)
}


#' Load posteriors from an explicit path (file or directory)
#'
#' @param path file path or directory path
#' @return List with \code{prior.distns}, \code{trait.mcmc}, \code{is.pda}
#' @keywords internal
load.posteriors.from.path <- function(path) {
  result <- list(
    prior.distns = NULL,
    trait.mcmc = NULL,
    is.pda = FALSE
  )

  if (!file.exists(path)) {
    PEcAn.logger::logger.error(
      "Posterior path does not exist: ", path
    )
    return(result)
  }

  # Collect list of .Rdata files to scan
  if (dir.exists(path)) {
    rdata_files <- list.files(
      path,
      pattern = "\\.[Rr][Dd]ata$",
      full.names = TRUE
    )
    if (length(rdata_files) == 0) {
      PEcAn.logger::logger.warn(
        "No .Rdata files found in posterior directory: ", path
      )
      return(result)
    }
  } else {
    rdata_files <- path
  }

  # Load each file and detect posterior type by contents
  for (f in rdata_files) {
    env <- new.env(parent = emptyenv())
    tryCatch(
      load(f, envir = env),
      error = function(e) {
        PEcAn.logger::logger.warn(
          "Failed to load posterior file: ", f, " -- ", conditionMessage(e)
        )
      }
    )

    # Detect Monte Carlo samples
    if (exists("trait.mcmc", envir = env, inherits = FALSE)) {
      PEcAn.logger::logger.info(
        "Found Monte Carlo samples (trait.mcmc) in: ", f
      )
      result$trait.mcmc <- env$trait.mcmc
    }

    # Detect distribution summaries (post.distns takes priority naming,but prior.distns also accepted)
    if (exists("post.distns", envir = env, inherits = FALSE)) {
      PEcAn.logger::logger.info(
        "Found distribution summaries (post.distns) in: ", f
      )
      # Only set if not already populated by an earlier file
      if (is.null(result$prior.distns)) {
        result$prior.distns <- env$post.distns
      }
    }
    if (exists("prior.distns", envir = env, inherits = FALSE)) {
      PEcAn.logger::logger.info(
        "Found distribution summaries (prior.distns) in: ", f
      )
      if (is.null(result$prior.distns)) {
        result$prior.distns <- env$prior.distns
      }
    }
  }

  # Log precedence outcome
  if (!is.null(result$trait.mcmc) && !is.null(result$prior.distns)) {
    PEcAn.logger::logger.info(
      "Both Monte Carlo samples and distribution summaries found. ",
      "Monte Carlo samples will take precedence."
    )
  }

  return(result)
}


#' Legacy fallback: load posteriors from outdir and/or database
#'
#' @param outdir PFT output directory
#' @param posteriorid Posterior ID for database lookup
#' @param con db connection
#' @param hostname host name for dbfile lookup
#' @return List with \code{prior.distns}, \code{trait.mcmc}, \code{is.pda}
#' @keywords internal
load.posteriors.legacy <- function(outdir = NULL,
                                   posteriorid = NULL,
                                   con = NULL,
                                   hostname = NULL) {
  result <- list(
    prior.distns = NULL,
    trait.mcmc = NULL,
    is.pda = FALSE
  )

  ## Step 1: Load distribution summaries from outdir
  if (!is.null(outdir)) {
    fname <- file.path(outdir, "post.distns.Rdata")
    if (file.exists(fname)) {
      env <- new.env(parent = emptyenv())
      load(fname, envir = env)
      if (exists("post.distns", envir = env, inherits = FALSE)) {
        result$prior.distns <- env$post.distns
      }
    } else {
      prior_fname <- file.path(outdir, "prior.distns.Rdata")
      if (file.exists(prior_fname)) {
        env <- new.env(parent = emptyenv())
        load(prior_fname, envir = env)
        if (exists("prior.distns", envir = env, inherits = FALSE)) {
          result$prior.distns <- env$prior.distns
        }
      }
    }
  }

  ## Step 2: Try database lookup for MCMC files
  if (!is.null(posteriorid) && !is.null(con)) {
    files <- tryCatch(
      PEcAn.DB::dbfile.check(
        "Posterior", posteriorid,
        con, hostname,
        return.all = TRUE
      ),
      error = function(e) NULL
    )

    if (!is.null(files) && nrow(files) > 0) {
      # Scan all associated .Rdata files for trait.mcmc by content
      rdata_idx <- grep("\\.[Rr][Dd]ata$", files$file_name)
      for (idx in rdata_idx) {
        fpath <- file.path(files$file_path[idx], files$file_name[idx])
        if (file.exists(fpath)) {
          env <- new.env(parent = emptyenv())
          tryCatch(
            load(fpath, envir = env),
            error = function(e) {
              PEcAn.logger::logger.warn(
                "Failed to load DB posterior file: ", fpath,
                " -- ", conditionMessage(e)
              )
            }
          )
          if (exists("trait.mcmc", envir = env, inherits = FALSE)) {
            PEcAn.logger::logger.info(
              "Found Monte Carlo samples in DB-referenced file: ", fpath
            )
            result$trait.mcmc <- env$trait.mcmc
            # Detect PDA samples to preserve correlations
            if (exists("post.distns", envir = env, inherits = FALSE)) {
              result$prior.distns <- result$prior.distns %||% env$post.distns
            }
            # Check if this is a PDA file (content-based: presence of pda-specific markers,
            # or fall back to filename heuristic for backward compat)
            fname_lower <- tolower(files$file_name[idx])
            if (grepl("pda", fname_lower)) {
              result$is.pda <- TRUE
            }
            break # Use first MCMC found
          }
        }
      }
    }
  }

  ## Step 3: If no MCMC from DB, scan outdir for .Rdata files with trait.mcmc
  if (is.null(result$trait.mcmc) && !is.null(outdir) && dir.exists(outdir)) {
    rdata_files <- list.files(
      outdir,
      pattern = "\\.[Rr][Dd]ata$",
      full.names = TRUE
    )
    for (f in rdata_files) {
      env <- new.env(parent = emptyenv())
      tryCatch(
        load(f, envir = env),
        error = function(e) NULL
      )
      if (exists("trait.mcmc", envir = env, inherits = FALSE)) {
        PEcAn.logger::logger.info(
          "Found Monte Carlo samples in outdir file: ", f
        )
        result$trait.mcmc <- env$trait.mcmc
        fname_lower <- tolower(basename(f))
        if (grepl("pda", fname_lower)) {
          result$is.pda <- TRUE
        }
        break
      }
    }
  }

  return(result)
}
