#' Merge model outputted netCDF files by time steps specified by pecan settings file.
#' @details
#' The function is only tested for SIPNET model runs.
#' Please make sure you have the same netCDF formats if you want to proceed with different models.
#' We could also have more functions that deal with different dimensions (e.g., by site instead of by year).
#' 
#' @param settings.dir character: physical path to the pecan standard multi-settings file.
#' @param nc.outdir  character: physical path to the folder that contains the merged netCDF files.
#'
#' @return character: file paths to the merged netCDF files.
#' @export
#' 
#' @author Dongchen Zhang
#' @importFrom purrr %>%
#' @importFrom foreach %dopar%
all_site_nc_merge_by_year <- function (settings.dir, nc.outdir) {
  # check shell environments.
  if (suppressWarnings(system2("which", "cdo", stdout = FALSE)) != 0) {
    PEcAn.logger::logger.info("The cdo function is not detected in shell command.")
    return(NA)
  }
  # read settings.
  settings <- PEcAn.settings::read.settings(settings.dir)
  # grab model outdir.
  model.outdir <- settings$modeloutdir
  # grab ensemble size.
  ens.num <- settings$ensemble$size %>% as.numeric
  # grab number of CPUs for parallel computation.
  cores <- as.numeric(settings$state.data.assimilation$batch.settings$general.job$cores)
  # if we didn't assign number of CPUs in the settings.
  if (is.null(cores)) {
    cores <- parallel::detectCores() - 1
    # if we only have one CPU.
    if (cores < 1) cores <- 1
  }
  # grab site info.
  site_info <- settings %>% purrr::map(~.x[["run"]]) %>% 
    purrr::map("site") %>% purrr::map(function(s) {
      temp <- as.numeric(c(s$id, s$lon, s$lat))
      names(temp) <- c("site_id", "lon", "lat")
      temp
    }) %>% dplyr::bind_rows() %>% as.data.frame()
  # grab time points.
  time.points <- lubridate::year(seq(lubridate::date(settings$state.data.assimilation$start.date), 
                                     lubridate::date(settings$state.data.assimilation$end.date), 
                                     paste0("1 ", settings$state.data.assimilation$forecast.time.step)))
  # loop over time.
  # initialize parallel.
  cl <- parallel::makeCluster(as.numeric(cores))
  doSNOW::registerDoSNOW(cl)
  #progress bar
  pb <- utils::txtProgressBar(min = 1, max = length(site_info$site_id), style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  # record nc paths.
  nc.paths <- c()
  for (t in seq_along(time.points)) {
    time <- time.points[t] # grab the current time point.
    # loop over sites.
    nc.files <- 
      foreach::foreach(s = seq_along(site_info$site_id), 
                       .packages = c("Kendall", "purrr", "ncdf4"), 
                       .options.snow=opts) %dopar% {
                         single_site_nc_merge(model.outdir = model.outdir, 
                                              nc.outdir = nc.outdir, 
                                              ens.num = ens.num, 
                                              # cdo collgrid only works for numeric data type.
                                              site.id = as.numeric(site_info$site_id[s]), 
                                              lat = site_info$lat[s], 
                                              lon = site_info$lon[s], 
                                              time)
                       } %>% unlist
    # merge across sites using CDO command.
    cmd <- "cdo -P @CORES@ collgrid @NC.OUTDIR@/*.nc @OUTFILE@"
    cmd <- gsub("@CORES@", cores, cmd)
    cmd <- gsub("@NC.OUTDIR@", nc.outdir, cmd)
    cmd <- gsub("@OUTFILE@", file.path(nc.outdir, paste0(time, ".nc")), cmd)
    out <- system(cmd, intern = TRUE)
    # record the current nc path.
    nc.paths <- c(nc.paths, file.path(nc.outdir, paste0(time, ".nc")))
    # remove nc files for each site.
    unlink(nc.files)
  }
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  # return nc paths.
  return(nc.paths)
}

#' Merge model outputted netCDF files across ensembles for a single site.
#' @details
#' The function is only tested for SIPNET model runs.
#' Please make sure you have the same netCDF formats if you want to proceed with different models.
#' 
#' @param model.outdir character: physical path to the model output folder.
#' @param nc.outdir  character: physical path to the folder that contains the merged netCDF files.
#' @param ens.num numeric: ensemble size.
#' @param site.id numeric: identification number of the site.
#' @param lat numeric: latitude of the site.
#' @param lon numeric: longitude of the site.
#' @return character: file path to the merged netCDF file.
#' @export
#' 
#' @author Dongchen Zhang
single_site_nc_merge <- function (model.outdir, nc.outdir, ens.num, site.id, lat, lon, time) {
  # grab basic formats from the first nc file of the site.
  # create the folder name associated with first ensemble and first site.
  prefix <- "ENS-"
  folder.name <- paste0(prefix, sprintf("%05d", 1), "-", 1)
  # read nc file.
  nc <- ncdf4::nc_open(file.path(model.outdir, folder.name, paste0(time, ".nc")))
  nc.vars <- nc$var # grab variable definitions.
  time.values <- nc$dim$time # grab time dimensions.
  ncdf4::nc_close(nc) # close nc connection.
  # dimension and variable definitions.
  # site dimension.
  site_dim <- ncdf4::ncdim_def("site", units = "", vals = site.id)
  # time dimension.
  time_dim <- ncdf4::ncdim_def("time", longname = "time", units = time.values$units, vals = time.values$vals)
  # ensemble dimension.
  ens_dim <- ncdf4::ncdim_def("ensemble", longname = "ensemble member", unit = "", vals = 1:ens.num)
  # define site-specific variables.
  lat_var <- ncdf4::ncvar_def("latitude", units = "degrees_north", dim = site_dim, prec = "double")
  lon_var <- ncdf4::ncvar_def("longitude", units = "degrees_east", dim = site_dim, prec = "double")
  site_id_var <- ncdf4::ncvar_def("site_id", units = "", dim = site_dim, prec = "integer")
  # loop over variables.
  for (i in seq_along(nc.vars)) {
    # grab the variable name.
    var <- nc.vars[[i]]$name
    # skip if it's time related variable.
    if (grepl("time", var, fixed = T)) next
    # loop over ensembles.
    var.mat <- matrix(NA, time.values$len, ens.num)
    for (ens in 1:ens.num) {
      folder.name <- paste0(prefix, sprintf("%05d", ens), "-", site.id)
      nc <- ncdf4::nc_open(file.path(model.outdir, folder.name, paste0(time, ".nc")))
      var.mat[,ens] <- ncdf4::ncvar_get(nc, var = var)
      ncdf4::nc_close(nc)
    }
    # define the current SIPNET variable.
    temp_var <- ncdf4::ncvar_def(nc.vars[[i]]$name, 
                                 units = nc.vars[[i]]$units, 
                                 dim = list(site_dim, ens_dim, time_dim), 
                                 prec = nc.vars[[i]]$prec)
    # if it's the first variable, we will need to create the NC file along with the site-specific variables.
    if (i == 1) {
      # create nc file.
      nc_file <- ncdf4::nc_create(file.path(nc.outdir, paste0(site.id, "_", time, ".nc")), list(site_id_var, lon_var, lat_var, temp_var))
      # add the site-specific variables.
      ncdf4::ncvar_put(nc_file, varid = "site_id", vals = site.id)
      ncdf4::ncvar_put(nc_file, varid = "latitude", vals = lat)
      ncdf4::ncvar_put(nc_file, varid = "longitude", vals = lon)
      # add the current variable.
      ncdf4::ncvar_put(nc_file, varid = nc.vars[[i]]$name, vals = var.mat)
    } else {
      # add additional variable.
      nc_file <- ncdf4::ncvar_add(nc_file, temp_var)
      # update data.
      ncdf4::ncvar_put(nc_file, varid = nc.vars[[i]]$name, vals = var.mat)
    }
  }
  # close nc connection.
  ncdf4::nc_close(nc_file)
  # return all nc paths.
  return(file.path(nc.outdir, paste0(site.id, "_", time, ".nc")))
}