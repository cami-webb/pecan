##' Convert PEPRMT output to netCDF
##'
##' Converts all output contained in a folder to netCDF.
##' @name model2netcdf.PEPRMT
##' @title Function to convert PEPRMT model output to standard netCDF format
##' @param outdir Location of PEPRMT model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##' @author Abigail Lewis
model2netcdf.PEPRMT <- function(outdir, sitelat, sitelon, start_date, end_date) {
  runid <- basename(outdir)
  PEPRMT.configs <- utils::read.table(file.path(gsub(pattern = "/out/",
                                             replacement = "/run/", x = outdir),
                                        paste0("CONFIG.", runid)),
                              stringsAsFactors = FALSE)
  
  ### Read in model output in PEPRMT format
  PEPRMT.output      <- utils::read.table(file.path(outdir, "out.txt"),
                                  header = FALSE, sep = "")
  PEPRMT.output.dims <- dim(PEPRMT.output)
  
  ### Determine number of years and output timestep
  days       <- as.Date(start_date):as.Date(end_date)
  year       <- strftime(as.Date(days, origin = "1970-01-01"), "%Y")
  num.years  <- length(unique(year))
  years      <- unique(year)
  timestep.s <- 86400
  
  ### Loop over years in PEPRMT output to create separate netCDF outputs
  for (y in years) {
    if (file.exists(file.path(outdir, paste(y, "nc", sep = ".")))) {
      next
    }
    print(paste("---- Processing year: ", y))  #turn on for debugging
    
    ## Subset data for processing
    sub.PEPRMT.output <- subset(PEPRMT.output, year == y)
    sub.PEPRMT.output.dims <- dim(sub.PEPRMT.output)

    # ******************** Declare netCDF variables ********************#
    start.day <- 1
    if (y == lubridate::year(start_date)){
      start.day <- yday(start_date)
    } 
    tvals <- (start.day:sub.PEPRMT.output.dims[1])-1
    bounds <- array(data=NA, dim=c(length(tvals),2))
    bounds[,1] <- tvals
    bounds[,2] <- bounds[,1]+1
    t   <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", y, "-01-01 00:00:00"), 
                     vals = tvals, calendar = "standard", unlim = TRUE)
    ## ***** Need to dynamically update the UTC offset here *****
    
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
    dims <- list(lon = lon, lat = lat, time = t)
    time_interval <- ncdf4::ncdim_def(name = "hist_interval", 
                                      longname="history time interval endpoint dimensions",
                                      vals = 1:2, units="")
    
    ## Output names
    # SOM_total
    # SOM_labile
    # GPP_mod (units)
    # Plant_flux_net (units)
    # Hydro_flux (units)
    # CH4_mod (units)
    
    # names(sub.PEPRMT.output) <- c("SOM_total", "SOM_labile", "GPP_mod", 
    #"Plant_flux_net", "Hydro_flux", "CH4_mod")
    
    ## Setup outputs for netCDF file in appropriate units
    output <- list()
    ## Fluxes
    output[[1]] <- (sub.PEPRMT.output[, 1] * 0.001)/timestep.s  # Autotrophic Respiration in kgC/m2/s
    output[[2]] <- (sub.PEPRMT.output[, 21] + sub.PEPRMT.output[, 23]) * 0.001 / timestep.s  # Heterotrophic Resp kgC/m2/s
    output[[3]] <- (sub.PEPRMT.output[, 31] * 0.001)/timestep.s  # GPP in kgC/m2/s    
    output[[4]] <- (sub.PEPRMT.output[, 33] * 0.001)/timestep.s  # NEE in kgC/m2/s
    output[[5]] <- (sub.PEPRMT.output[, 3] + sub.PEPRMT.output[, 5] + sub.PEPRMT.output[, 7]) * 0.001/timestep.s  # NPP kgC/m2/s
    output[[6]] <- (sub.PEPRMT.output[, 9] * 0.001) / timestep.s  # Leaf Litter Flux, kgC/m2/s
    output[[7]] <- (sub.PEPRMT.output[, 11] * 0.001) / timestep.s  # Woody Litter Flux, kgC/m2/s
    output[[8]] <- (sub.PEPRMT.output[, 13] * 0.001) / timestep.s  # Root Litter Flux, kgC/m2/s
    
    ## Pools
    output[[9]]  <- (sub.PEPRMT.output[, 15] * 0.001)  # Leaf Carbon, kgC/m2
    output[[10]] <- (sub.PEPRMT.output[, 17] * 0.001)  # Wood Carbon, kgC/m2
    output[[11]] <- (sub.PEPRMT.output[, 19] * 0.001)  # Root Carbon, kgC/m2
    output[[12]] <- (sub.PEPRMT.output[, 27] * 0.001)  # Litter Carbon, kgC/m2
    output[[13]] <- (sub.PEPRMT.output[, 29] * 0.001)  # Soil Carbon, kgC/m2
    
    ## time_bounds
    output[[18]] <- c(rbind(bounds[,1], bounds[,2]))
    
    ## missing value handling
    for (i in seq_along(output)) {
      if (length(output[[i]]) == 0) 
        output[[i]] <- rep(-999, length(t$vals))
    }
    
    ## setup nc file
    # ******************** Declar netCDF variables ********************#
    nc_var <- list()
    nc_var[[1]]  <- PEcAn.utils::to_ncvar("AutoResp", dims)
    nc_var[[2]]  <- PEcAn.utils::to_ncvar("HeteroResp", dims)
    nc_var[[3]]  <- PEcAn.utils::to_ncvar("GPP", dims)
    nc_var[[4]]  <- PEcAn.utils::to_ncvar("NEE", dims)
    nc_var[[5]]  <- PEcAn.utils::to_ncvar("NPP", dims)
    nc_var[[6]]  <- PEcAn.utils::to_ncvar("leaf_litter_carbon_flux", dims) #was LeafLitter
    nc_var[[7]]  <- PEcAn.utils::to_ncvar("WoodyLitter", dims) #need to resolve standard woody litter flux
    nc_var[[8]]  <- PEcAn.utils::to_ncvar("subsurface_litter_carbon_flux", dims) #was RootLitter
    nc_var[[9]]  <- PEcAn.utils::to_ncvar("leaf_carbon_content", dims) #was LeafBiomass
    nc_var[[10]] <- PEcAn.utils::to_ncvar("wood_carbon_content", dims) #was WoodBiomass
    nc_var[[11]] <- PEcAn.utils::to_ncvar("root_carbon_content", dims) #was RootBiomass
    nc_var[[12]] <- PEcAn.utils::to_ncvar("litter_carbon_content", dims) #was LitterBiomass
    nc_var[[13]] <- PEcAn.utils::to_ncvar("soil_carbon_content", dims) #was SoilC; SOM pool technically includes woody debris (can't be represented by our standard)
    
    nc_var[[14]] <- PEcAn.utils::to_ncvar("TotalResp", dims)
    nc_var[[15]] <- PEcAn.utils::to_ncvar("TotLivBiom", dims)
    nc_var[[16]] <- PEcAn.utils::to_ncvar("TotSoilCarb", dims)
    nc_var[[17]] <- PEcAn.utils::to_ncvar("LAI", dims)
    nc_var[[18]] <- ncdf4::ncvar_def(name="time_bounds", units='', 
                                     longname = "history time interval endpoints", dim=list(time_interval,time = t), 
                                     prec = "double")
    
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    ncdf4::ncatt_put(nc, "time", "bounds", "time_bounds", prec=NA)
    for (i in seq_along(nc_var)) {
      ncdf4::ncvar_put(nc, nc_var[[i]], output[[i]])
    }
    ncdf4::nc_close(nc)
    
  }  ### End of year loop
} # model2netcdf.PEPRMT
# ==================================================================================================#
## EOF
