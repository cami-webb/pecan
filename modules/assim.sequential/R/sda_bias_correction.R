#' @description
#' This function helps to correct the forecasts' biases based on 
#' ML (random forest) training on the previous time point.
#' @title sda_bias_correction
#' 
#' @param site.locs data.frame: data.frame that contains longitude and latitude in its first and second column.
#' @param t numeric: the current number of time points (e.g., t=1 for the beginning time point).
#' @param all.X list: lists of data frame of model forecast from the beginning to the current time points 
#' that has n (ensemble size) rows and n.var (number of variables) times n.site (number of locations) columns.
#' (e.g., 100 ensembles, 4 variables, and 8,000 locations will end up with data.frame of 100 rows and 32,000 columns)
#' @param obs.mean List: lists of date times named by time points, which contains lists of sites named by site ids, 
#' which contains observation means for each state variables of each site for each time point.
#' @param state.interval matrix: containing the upper and lower boundaries for each state variable.
#' @param cov.dir character: physical path to the directory contains the time series covariate maps.
#' @param py.init R function: R function to initialize the python functions. Default is NULL.
#' the default random forest will be used if `py.init` is NULL.
#' @param pre.states list: containing previous covariates for each location.
#'
#' @return list: the current X after the bias-correction; the ML model for each variable; predicted residuals.
#' 
#' @author Dongchen Zhang
#' @importFrom dplyr %>%

sda_bias_correction <- function (site.locs, 
                                 t, all.X, 
                                 obs.mean, 
                                 state.interval, 
                                 cov.dir, 
                                 pre.states,
                                 py.init = NULL) {
  # initialize X.
  X <- all.X[[t]]
  pre.X <- all.X[[t-1]]
  pre.pre.X <- all.X[[t-2]]
  # if we have prescribed python script to use.
  if (!is.null(py.init)) {
    # load python functions.
    py <- py.init()
  }
  # grab variable names.
  var.names <- rownames(state.interval)
  # create terra spatial points.
  pts <- terra::vect(cbind(site.locs$Lon, site.locs$Lat), crs = "epsg:4326")
  # grab the current year.
  y <- lubridate::year(names(obs.mean))[t]
  # if we don't have previous extracted information.
  # grab the covariate file path.
  cov.file.pre <- list.files(cov.dir, full.names = T)[which(grepl(y-1, list.files(cov.dir)))] # previous covaraites.
  # extract covariates for the previous time point.
  cov.pre <- terra::extract(x = terra::rast(cov.file.pre), y = pts)[,-1] # remove the first ID column.
  # factorize land cover band.
  if ("LC" %in% colnames(cov.pre)) {
    cov.pre[,"LC"] <- factor(cov.pre[,"LC"])
  }
  # loop over variables.
  # initialize model list for each variable.
  models <- res.vars <- vector("list", length = length(var.names)) %>% purrr::set_names(var.names)
  for (v in var.names) {
    # match variable and data names.
    site.inds <- data.source <- c()
    for (s in seq_along(pts)) {
      # match variable name.
      v.ind.pre <- which(names(obs.mean[[t-1]][[s]]) == v)
      v.ind.t <- which(names(obs.mean[[t]][[s]]) == v)
      if (length(v.ind.pre) > 0 &
          length(v.ind.t) > 0) {
        # match data name.
        a.pre <- attributes(obs.mean[[t-1]][[s]])
        a.pre <- a.pre$Source[v.ind.pre]
        a.t <- attributes(obs.mean[[t]][[s]])
        a.t <- a.t$Source[v.ind.t]
        if (a.pre == a.t) {
          site.inds[s] <- 1
        } else {
          site.inds[s] <- 0
        }
      } else {
        site.inds[s] <- 0
      }
      # record the current data source.
      if (site.inds[s] == 0) {
        data.source <- c(data.source, NA)
      } else {
        data.source <- c(data.source, a.t)
      }
    }
    # skip this variable if there is no data to be training.
    if (all(is.na(data.source))) next
    # train residuals on the previous time point.
    # grab column index for the current variable.
    inds <- which(grepl(v, colnames(pre.X)))
    # grab observations for the current variable.
    obs.v <- obs.mean[[t-1]] %>% purrr::map(function(obs){
      if (is.null(obs[[v]])) {
        return(NA)
      } else {
        return(obs[[v]])
      }
    }) %>% unlist
    # calculate residuals for the previous time point.
    res.pre <- colMeans(pre.X[,inds]) - obs.v
    # grab observations for the current variable.
    obs.v.pre <- obs.mean[[t-2]] %>% purrr::map(function(obs){
      if (is.null(obs[[v]])) {
        return(NA)
      } else {
        return(obs[[v]])
      }
    }) %>% unlist
    # calculate residuals for the previous time point.
    res.pre.pre <- colMeans(pre.pre.X[,inds]) - obs.v.pre
    # prepare training data set.
    ml.df <- cbind(cov.pre, res.pre.pre, colMeans(pre.X)[inds], res.pre)
    # prepare predicting covariates.
    # extract covariates for the current time point.
    cov.file <- list.files(cov.dir, full.names = T)[which(grepl(y, list.files(cov.dir)))] # current covaraites.
    cov.current <- terra::extract(x = terra::rast(cov.file), y = pts)[,-1] # remove the first ID column.
    # factorize land cover band.
    if ("LC" %in% colnames(cov.current)) {
      cov.current[,"LC"] <- factor(cov.current[,"LC"])
    }
    # prepare predicting data set.
    cov.df <- cbind(cov.current, res.pre, colMeans(X)[inds])
    # loop over data sources.
    res.all <- c() # initialize the entire residual errors.
    for (s in unique(data.source)) {
      if (is.na(s)) next # if there is no observation.
      message(paste("processing", s, v)) # report progress.
      # grab sites that have matched observations across time.
      s.inds <- which(site.inds == 1 & data.source == s)
      # keep sites that match variable and data names.
      s.df <- ml.df[s.inds,]
      colnames(s.df)[length(s.df)-1] <- "raw_dat" # rename the column name.
      colnames(s.df)[length(s.df)-2] <- "res_lag" # rename the column name.
      s.df <- rbind(pre.states[[v]][[s]], s.df) # grab previous covariates.
      s.df <- s.df[which(stats::complete.cases(s.df)),]
      if (nrow(s.df) == 0) next # jump to the next loop if we have zero records.
      pre.states[[v]][[s]] <- s.df # store the historical covariates for future use.
      # keep sites that match variable and data sources.
      s.cov.df <- cov.df[s.inds,]
      complete.inds <- which(stats::complete.cases(s.cov.df))
      s.cov.df <- s.cov.df[complete.inds,]
      colnames(s.cov.df)[length(s.cov.df)] <- "raw_dat"
      colnames(s.cov.df)[length(s.cov.df)-1] <- "res_lag"
      cov.names <- colnames(s.cov.df) # grab band names for the covariate map.
      if (is.null(py.init)) {
        # random forest training.
        formula <- stats::as.formula(paste("res.pre", "~", paste(cov.names, collapse = " + ")))
        model <- randomForest::randomForest(formula,
                                            data = s.df,
                                            ntree = 1000,
                                            na.action = stats::na.omit,
                                            keep.forest = TRUE,
                                            importance = TRUE)
        var.imp <- randomForest::importance(model)
        models[[v]][[s]] <- var.imp # store the variable importance.
        # predict residuals for the current time point.
        res.current <- stats::predict(model, s.cov.df)
      } else {
        # using functions from the python script.
        # training.
        fi_ret <- py$train_full_model(
          name = as.character(v), # current variable name.
          X = as.matrix(s.df[,-length(s.df)]), # covariates + previous forecast means.
          y = as.numeric(s.df[["res.pre"]]), # residuals.
          feature_names = colnames(s.df[,-length(s.df)])
        )
        # predicting.
        res.current <- py$predict_residual(as.character(v), as.matrix(s.cov.df))
        # store model outputs.
        # weights.
        w_now <- try(py$get_model_weights(as.character(v)), silent = TRUE)
        w_now <- min(max(as.numeric(w_now), 0), 1)
        w_named <- c(KNN = w_now, TREE = 1 - w_now)
        # var importance.
        fi_ret <- tryCatch(reticulate::py_to_r(fi_ret), error = function(e) fi_ret)
        fn <- as.character(unlist(fi_ret[["names"]], use.names = FALSE))
        fv <- as.numeric(unlist(fi_ret[["importances"]], use.names = FALSE)) %>% purrr::set_names(fn)
        models[[v]][[s]] <- list(weights = w_named, var.imp = fv) # store the variable importance.
      }
      res.all <- c(res.all, res.current)
    }
    # assign NAs to places with no observations in the previous time point.
    res <- rep(NA, length(obs.v)) %>% purrr::set_names(unique(attributes(X)$Site))
    res[complete.inds] <- res.all
    res[which(is.na(obs.v))] <- NA
    res.vars[[v]] <- res
    # correct the current forecasts.
    for (i in seq_along(inds)) {
      if (is.na(res[i])) next
      X[,inds[i]] <- X[,inds[i]] - res[i]
    }
  }
  # map forecasts towards the prescribed variable boundaries.
  for(i in 1:ncol(X)){
    int.save <- state.interval[which(startsWith(colnames(X)[i], var.names)),]
    X[X[,i] < int.save[1],i] <- int.save[1]
    X[X[,i] > int.save[2],i] <- int.save[2]
  }
  return(list(X = X, models = models, res = res.vars, pre.states = pre.states))
}