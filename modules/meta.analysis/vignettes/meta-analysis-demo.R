#' ---
#' title: "Basic meta-analysis example"
#' output: rmarkdown::html_vignette()
#' vignette:
#'    %\VignetteIndexEntry{BAsic meta-analysis example}
#'    %\VignetteEngine{knitr::rmarkdown}
#'    %\VignetteEncoding{UTF-8}
#' ---

if (interactive()) {
  devtools::load_all("./modules/meta.analysis")
} else {
  use("PEcAn.MA")
}

use("tibble", c("tribble", "as_data_frame"))

#' An example of a prior distributions table, demonstrating the expected format.

prior_distns <- tribble(
  ~row_name                     , ~distn    , ~parama , ~paramb , ~n  , #nolint
  "growth_resp_factor"          , "beta"    ,  2.63   ,   6.52  ,   0 , #nolint
  "leaf_turnover_rate"          , "weibull" ,  1.37   ,   1.43  , 363 , #nolint
  "root_respiration_rate"       , "unif"    ,  0.00   , 100.00  , NA  , #nolint
  "root_turnover_rate"          , "unif"    ,  0.00   ,  10.00  , NA  , #nolint
  "Amax"                        , "unif"    ,  0.00   ,  40.00  , NA  , #nolint
  "leaf_respiration_rate_m2"    , "weibull" ,  2.00   ,   6.00  , NA  , #nolint
  "SLA"                         , "lnorm"   ,  1.89   ,   0.61  , 455 , #nolint
  "leafC"                       , "norm"    , 50.60   ,   1.32  , 291 , #nolint
  "Vm_low_temp"                 , "norm"    ,  0.00   ,   3.00  , NA  , #nolint
  "AmaxFrac"                    , "unif"    ,  0.60   ,   0.90  , NA  , #nolint
  "psnTOpt"                     , "unif"    ,  5.00   ,  40.00  , NA  , #nolint
  "stem_respiration_rate"       , "unif"    ,  0.00   , 100.00  , NA  , #nolint
  "extinction_coefficient"      , "unif"    ,  0.38   ,   0.62  , NA  , #nolint
  "half_saturation_PAR"         , "unif"    ,  4.00   ,  27.00  , NA  , #nolint
  "dVPDSlope"                   , "unif"    ,  0.01   ,   0.25  , NA  , #nolint
  "dVpdExp"                     , "unif"    ,  1.00   ,   3.00  , NA  , #nolint
  "veg_respiration_Q10"         , "unif"    ,  1.40   ,   2.60  , NA  , #nolint
  "fine_root_respiration_Q10"   , "unif"    ,  1.40   ,   5.00  , NA  , #nolint
  "coarse_root_respiration_Q10" , "unif"    ,  1.40   ,   5.00  , NA  , #nolint
)

#' `PEcAn.MA` expects the code to be a base R `data.frame` with trait names as row names.
#' Below, we convert the above `tibble` to this format.

priors <- as.data.frame(prior_distns)
rownames(priors) <- priors[["row_name"]]
priors[["row_name"]] <- NULL

#' The format of the `trait_data` is a named list of `data.frame`s,
#' with each `data.frame` containing data for the corresponding trait (for that PFT).
#' PEcAn database queries return all the columns the meta-analysis needs and more, formatted as expected,
#' but only the following columns are expected:
#'    - `name` (character) -- A string description of the trait
#'    - `mean` (numeric) -- The mean value of the trait measurement (or the only value, if only one value is given)
#'    - `greenhouse` (boolean) -- `TRUE` if from a greenhouse; `FALSE` if not (e.g., natural setting)
#'    - `stat` (character) -- error statistic type
#'    - `n` (integer or NA) -- sample size
#'    - `site_id` (integer) -- site ID (used for grouping)
#'    - `specie_id` (integer) -- species ID (as above)
#'    - `citation_id` (integer) -- citation ID (as above)
#'    - `cultivar_id` (integer) -- cultivar ID (as above)
#'    - `date` (datetime) -- date of trait observation (as above)
#'    - `time` (datetime) -- time of trait observation (as above)
#'    - `control` (boolean) -- If `TRUE`, this is the "control" part of an
#'        experiment (or there is no experiment). If `FALSE`, this is the
#'        treatment.
#'
#' To avoid having to load custom data,
#' here is some simulated data that fits these criteria.

simulate_data <- function(n_rows, mean_lo, mean_hi, se_lo, se_hi) {
  return(data.frame(
    name = sprintf("t%02d", seq_len(n_rows)),
    mean = runif(n_rows, mean_lo, mean_hi),
    statname = "SE",
    stat = suppressWarnings(runif(n_rows, se_lo, se_hi)),
    greenhouse = sample(
      c(TRUE, FALSE),
      size = n_rows,
      replace = TRUE,
      prob = c(0.1, 0.9)
    ),
    n = sample(1:100, size = n_rows, replace = TRUE),
    site_id = sample(1:10, size = n_rows, replace = TRUE),
    specie_id = sample(1:4, size = n_rows, replace = TRUE),
    citation_id = sample(1:8, size = n_rows, replace = TRUE),
    treatment_id = sample(1:3, size = n_rows, replace = TRUE),
    control = TRUE,
    date = NA,
    time = NA,
    cultivar_id = NA
  ))
}

n_rows <- 40
trait_data <- list()
trait_data[["Amax"]] <- simulate_data(n_rows, 4.0, 20.0, 0.13, 4.0)
trait_data[["SLA"]] <- simulate_data(n_rows, 3.0, 15.0, 0.06, 1.7)
trait_data[["leaf_turnover_rate"]] <- simulate_data(n_rows, 0.1, 0.5, 0.01, 0.05)

#' Run the meta analysis, performing prior and posterior checks and summarizing the results.

ma_result <- run_meta_analysis_pft(
  trait_data,
  priors,
  iterations = 1000,
  pft_name = "temperate.coniferous",
  outdir = "_ma-test"
)

print(names(ma_result))

#' This returns three things: (1) The posterior distributions...

print(ma_result[["post.distns"]])

#' ...(2) the full MCMC samples (as a list)

print(tail(ma_result[["trait.mcmc"]][["Amax"]][[1]]))

#' ...(3) and the "JAGS-ified" trait data.

head(ma_result[["jagged.data"]][["Amax"]])
