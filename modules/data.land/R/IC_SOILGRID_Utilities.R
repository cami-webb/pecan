#' SoilGrids Initial Conditions (IC) Utilities
#' @description Functions for generating soil carbon IC files from SoilGrids250m data
#' @details This module provides functions for extracting, processing, and generating
#'          ensemble members for soil carbon initial conditions using SoilGrids data.
#'          All soil carbon values are in kg/m2.
#'
#' Process SoilGrids data for initial conditions
#' 
#' @param settings PEcAn settings list containing site information
#' @param dir Output directory for IC files
#' @param overwrite Overwrite existing files? (Default: FALSE)
#' @param verbose Print detailed progress information? (Default: FALSE)
#' 
#' @return List of paths to generated IC files, organized by site ID
#' @export
#'
#' @examples
#' \dontrun{
#' # From settings object
#' settings <- PEcAn.settings::read.settings("pecan.xml")
#' ic_files <- soilgrids_ic_process(settings, dir = "~/output/IC")  
#' }
#' @importFrom dplyr %>%
#' @author Akash
#'
soilgrids_ic_process <- function(settings, dir, overwrite = FALSE, verbose = FALSE) {
  start_time <- proc.time()
  
  site_info <- settings$run$site
  if (is.list(site_info) && !is.null(site_info$id)) {
    site_info <- list(site_info)
  }
  site_info <- site_info %>% 
    purrr::map(function(site) {
      site$lat <- as.numeric(site$lat)
      site$lon <- as.numeric(site$lon)
      str_id <- if (isTRUE(site$id > 1e9)) {
        paste0(site$id %/% 1e9, "-", site$id %% 1e9)
      } else {
        as.character(site$id)
      }
      data.frame(
        site_id = site$id,
        lat = site$lat,
        lon = site$lon,
        site_name = site$name,
        str_id = str_id,
        stringsAsFactors = FALSE
      )
    }) %>% 
    dplyr::bind_rows()

  n_sites <- nrow(site_info)
  if (n_sites == 0) {
    PEcAn.logger::logger.severe("No sites found in the provided input")
  }
  
  size <- ifelse(is.null(settings$ensemble$size), 1, settings$ensemble$size)
  
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  data_dir <- file.path(dir, "SoilGrids_data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  
  # Check for cached data
  soilc_csv_path <- file.path(data_dir, "soilgrids_soilC_data.csv")
  if (file.exists(soilc_csv_path) && !overwrite) {
    soil_data <- utils::read.csv(soilc_csv_path, check.names = FALSE)
  } else {
    soil_data <- PEcAn.data.land::soilgrids_soilC_extract(
      site_info = site_info,
      outdir = data_dir,
      verbose = verbose
    )
    # Save the extracted data for future use
    utils::write.csv(soil_data, soilc_csv_path, row.names = FALSE)
  }
  
  # Validate soil carbon data units through range check
  if (any(soil_data$`Total_soilC_0-30cm` > 150, na.rm = TRUE)) {
    PEcAn.logger::logger.warn("Some soil carbon values exceed 150 kg/m2, values may be in wrong units")
  }
  
  processed_data <- preprocess_soilgrids_data(soil_data, verbose)
  
  if (nrow(processed_data$data) == 0) {
    PEcAn.logger::logger.severe("No valid sites remain after preprocessing")
  }
  
  ens_files <- list()
  
  for (s in 1:nrow(processed_data$data)) {
    site_data <- processed_data$data[s, ]
    
    site_idx <- which(site_info$site_id == site_data$Site_ID)
    if (length(site_idx) == 0) {
      PEcAn.logger::logger.warn(sprintf("Site %s not found in site_info", site_data$Site_ID))
      next
    }
    current_site <- site_info[site_idx, ]
    
    # Create output directory for this site
    site_folder <- file.path(dir, paste0("SoilGrids_site_", current_site$str_id))
    if (!dir.exists(site_folder)) {
      dir.create(site_folder, recursive = TRUE)
    }
    
    # Check for existing files
    existing_files <- list.files(site_folder, "*.nc$", full.names = TRUE)
    if (length(existing_files) > 0 && !overwrite) {
      ens_files[[current_site$str_id]] <- existing_files
      next
    }
    
    # Generate all ensemble members
    ens_data_30cm <- generate_soilgrids_ensemble(
      processed_data = processed_data,
      site_id = current_site$site_id,
      size = size,
      depth_layer = "0-30cm",
      verbose = verbose
    )
    
    ens_data_200cm <- generate_soilgrids_ensemble(
      processed_data = processed_data,
      site_id = current_site$site_id,
      size = size,
      depth_layer = "0-200cm",
      verbose = verbose
    )
    
    site_files <- list()
    
    # Write each ensemble member to NetCDF files
    for (ens in seq_len(size)) {
      ens_input <- list(
        dims = list(
          lat = current_site$lat,
          lon = current_site$lon,
          time = 1,
          depth = c(0.3, 2.0) 
        ),
        vals = list(
          soil_organic_carbon_content = c(ens_data_30cm[ens], ens_data_200cm[ens]),
          wood_carbon_content = 0,
          litter_carbon_content = 0
        )
      )

      # Write to NetCDF file
      result <- PEcAn.data.land::pool_ic_list2netcdf(
        input = ens_input,
        outdir = site_folder,
        siteid = current_site$site_id,
        ens = ens
      )
      
      site_files[[ens]] <- result$file
    }
    
    ens_files[[current_site$str_id]] <- site_files
  }
  
  if (verbose) {
    end_time <- proc.time()
    elapsed_time <- end_time - start_time
    PEcAn.logger::logger.info(sprintf("IC generation completed for %d site(s) in %.2f seconds", 
                                    n_sites, elapsed_time[3]))
  }
  
  return(ens_files)
}

#' Preprocess SoilGrids data for ensemble generation
#'
#' @param soil_data Dataframe with SoilGrids soil carbon data
#' @param verbose Logical, print detailed progress information
#' 
#' @return List containing processed data and CV distributions for both depths
#' @export
preprocess_soilgrids_data <- function(soil_data, verbose = FALSE) {
  if (verbose) {
    PEcAn.logger::logger.info("Preprocessing soil carbon data following PEcAn standards")
  }

  # Only process sites with complete mean data for both depths
  complete_sites <- !is.na(soil_data$`Total_soilC_0-30cm`) & 
                   soil_data$`Total_soilC_0-30cm` > 0 &
                   !is.na(soil_data$`Total_soilC_0-200cm`) & 
                   soil_data$`Total_soilC_0-200cm` > 0

  if (!any(complete_sites)) {
    PEcAn.logger::logger.severe("No sites with complete data for both depth intervals found")
  }
  
  processed <- soil_data[complete_sites, ]
  
  if (verbose) {
    removed_count <- nrow(soil_data) - nrow(processed)
    PEcAn.logger::logger.info(sprintf("Removed %d site(s) with incomplete data. Processing %d sites", 
                                    removed_count, nrow(processed)))
  }
  
  # Calculate CV distributions
  depths <- list(
    "30cm" = list(mean_col = "Total_soilC_0-30cm", std_col = "Std_soilC_0-30cm"),
    "200cm" = list(mean_col = "Total_soilC_0-200cm", std_col = "Std_soilC_0-200cm")
  )
  
  cv_dist <- lapply(depths, function(depth_info) {
    valid_cv <- processed[[depth_info$mean_col]] > 0 & 
               !is.na(processed[[depth_info$std_col]]) & 
               processed[[depth_info$std_col]] > 0
    
    if (sum(valid_cv) < 5) {
      return(list(type = "none"))
    }
    
    cv_values <- processed[[depth_info$std_col]][valid_cv] / processed[[depth_info$mean_col]][valid_cv]
    cv_bounds <- quantile(cv_values, probs = c(0.05, 0.95), na.rm = TRUE)
    cv_filtered <- cv_values[cv_values >= cv_bounds[1] & cv_values <= cv_bounds[2]]
    
    if (length(cv_filtered) < 5) {
      return(list(type = "none"))
    }
    
    gamma_fit <- try(MASS::fitdistr(cv_filtered, "gamma"), silent = TRUE)
    if (!inherits(gamma_fit, "try-error")) {
      list(
        type = "gamma",
        shape = gamma_fit$estimate["shape"],
        rate = gamma_fit$estimate["rate"],
        bounds = as.vector(cv_bounds)
      )
    } else {
      list(
        type = "empirical",
        values = cv_filtered,
        bounds = as.vector(cv_bounds)
      )
    }
  })
  
  return(list(
    data = processed,
    cv_distribution_30cm = cv_dist[["30cm"]],
    cv_distribution_200cm = cv_dist[["200cm"]]
  ))
}

#' Generate soil carbon ensemble members for specific depth
#'
#' @param processed_data Output from preprocess_soilgrids_data()
#' @param site_id Target site ID
#' @param size Number of ensemble members to generate
#' @param depth_layer Depth layer ("0-30cm" or "0-200cm")
#' @param verbose Logical, print detailed progress information
#' @param seed Optional random seed for reproducibility
#' 
#' @return Vector of soil carbon values with proper uncertainty handling
#' @export
generate_soilgrids_ensemble <- function(processed_data, site_id, size, depth_layer, verbose = FALSE, seed = NULL) {
  if (verbose) {
    PEcAn.logger::logger.info(sprintf("Generating %d ensemble members for site %s (%s)",size, site_id, depth_layer))
  }
  
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      old_seed <- .Random.seed
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv))
    }
    set.seed(seed)
  }
  
  site_row <- which(processed_data$data$Site_ID == site_id)
  if (length(site_row) == 0) {
    PEcAn.logger::logger.severe(sprintf("Site %s not found in processed data", site_id))
  }
  
  # Select appropriate columns based on depth layer
  if (depth_layer == "0-30cm") {
    mean_c <- processed_data$data$`Total_soilC_0-30cm`[site_row]
    original_sd <- processed_data$data$`Std_soilC_0-30cm`[site_row]
    cv_dist <- processed_data$cv_distribution_30cm
  } else {
    mean_c <- processed_data$data$`Total_soilC_0-200cm`[site_row]
    original_sd <- processed_data$data$`Std_soilC_0-200cm`[site_row]
    cv_dist <- processed_data$cv_distribution_200cm
  }
  
  if (is.na(mean_c) || mean_c <= 0) {
    PEcAn.logger::logger.severe(sprintf("Invalid mean soil carbon value for site %s (%s)", 
                                      site_id, depth_layer))
  }
  
  soil_c_values <- numeric(size)
  
  # Use site-specific uncertainty
  if (!is.na(original_sd) && original_sd > 0) {
    shape <- (mean_c^2) / (original_sd^2)
    rate <- mean_c / (original_sd^2)
    if (is.finite(shape) && is.finite(rate) && shape > 0 && rate > 0) {
      soil_c_values <- pmax(stats::rgamma(size, shape, rate), 0)
    } else {
      soil_c_values <- rep(mean_c, size)
    }
  } else if (cv_dist$type != "none") {
    # Integrate over uncertainty using CV distribution
    if (cv_dist$type == "gamma") {
      cv_samples <- stats::rgamma(size, cv_dist$shape, cv_dist$rate)
      if (all(is.finite(cv_dist$bounds))) {
        cv_samples <- pmax(pmin(cv_samples, cv_dist$bounds[2]), cv_dist$bounds[1])
      }
    } else {
      cv_samples <- sample(cv_dist$values, size, replace = TRUE)
    }
    
    sd_values <- mean_c * cv_samples
    valid <- !is.na(sd_values) & sd_values > 0
    
    if (any(valid)) {
      shape_vec <- (mean_c^2) / (sd_values[valid]^2)
      rate_vec <- mean_c / (sd_values[valid]^2)
      soil_c_values[valid] <- pmax(stats::rgamma(sum(valid), shape_vec, rate_vec), 0)
      soil_c_values[!valid] <- mean_c
    } else {
      soil_c_values <- rep(mean_c, size)
    }
  } else {
    # Deterministic fallback
    soil_c_values <- rep(mean_c, size)
  }
  
  if (verbose) {
    PEcAn.logger::logger.debug(sprintf("Generated ensemble for site %s (%s): mean=%.2f, sd=%.2f",
      site_id, depth_layer, mean(soil_c_values), stats::sd(soil_c_values)
    ))
  }
  
  return(soil_c_values)
}