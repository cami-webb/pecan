make_sa_test_settings <- function() {
  list(
    outdir = "/fake/output/path", # not accessed when sa_samples is provided
    pfts = list(list(name = "pft1")),
    ensemble = list(
      samplingspace = list(
        parameters = list(method = "uniform"),
        met = list(method = "sampling")
      )
    )
  )
}

mock_sa_samples <- list(
  pft1 = structure(
    matrix(1:9, nrow = 3, ncol = 3),
    dimnames = list(c("25", "50", "75"), c("trait1", "trait2", "trait3"))
  )
)


# ---------------- tests: generate_OAT_SA_design------------------------------

test_that("generate_OAT_SA_design returns correct structure and run count", {
  settings <- make_sa_test_settings()
  
  result <- generate_OAT_SA_design(settings, sa_samples = mock_sa_samples)
  
  # 1 median + 3 traits * 2 non-median quantiles = 7
  expect_equal(nrow(result$X), 7)
  expect_true("param" %in% names(result$X))
  expect_true(is.data.frame(result$X))
})

test_that("generate_OAT_SA_design keeps param sequential and non-param constant at 1", {
  settings <- make_sa_test_settings()
  result <- generate_OAT_SA_design(settings, sa_samples = mock_sa_samples)
  
  expect_equal(result$X$param, seq_len(nrow(result$X)))

  non_param_cols <- setdiff(names(result$X), "param")
  for (col in non_param_cols) {
    expect_true(all(result$X[[!!col]] == 1))
  }
})


# ------------------ tests: generate_joint_ensemble_design -------------------

test_that("generate_joint_ensemble_design returns correct structure", {
  settings <- make_sa_test_settings()
  settings$run <- list(inputs = list(met = list(path = c("met1.nc", "met2.nc"))))
  
  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
    function(...) list(ids = sample(1:2, 5, replace = TRUE)))
  mockery::stub(generate_joint_ensemble_design, "get.parameter.samples",
    function(...) NULL)
  mockery::stub(generate_joint_ensemble_design, "file.exists",
    function(...) TRUE)
  
  result <- generate_joint_ensemble_design(settings, ensemble_size = 5)
  
  expect_true("X" %in% names(result))
  expect_equal(nrow(result$X), 5)
  expect_true("param" %in% names(result$X))
})

test_that("OAT design has constant inputs while ensemble design varies them", {
  # SA design already tested above - just get it for comparison
  settings <- make_sa_test_settings()
  sa_result <- generate_OAT_SA_design(settings, sa_samples = mock_sa_samples)
  
  # test that ensemble design STRUCTURE allows variation in non-param columns
  settings$run <- list(inputs = list(met = list(path = c("m1.nc", "m2.nc", "m3.nc"))))
  mockery::stub(generate_joint_ensemble_design, "input.ens.gen",
    function(...) list(ids = c(1, 2, 3, 1, 2))) # varied indices
  mockery::stub(generate_joint_ensemble_design, "get.parameter.samples",
    function(...) NULL)
  mockery::stub(generate_joint_ensemble_design, "file.exists",
    function(...) TRUE)
  
  ens_result <- generate_joint_ensemble_design(settings, ensemble_size = 5)
  
  # key comparison: SA has 1 unique value(constant), ensemble has multiple(varied)
  expect_equal(length(unique(sa_result$X$met)), 1)
  expect_true(length(unique(ens_result$X$met)) > 1)
})

#------------------ tests: OAT design with write.sa.configs -------------------
# verifies design produces output compatible with SA postprocessing

test_that("OAT design integrates with write.sa.configs for SA postprocessing", {
  # mock model config writer
  assign("write.config.FAKE", function(defaults, trait.values, settings, run.id) {
    invisible(NULL)
  }, envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv))
  
  workflow_root <- withr::local_tempdir()
  rundir <- file.path(workflow_root, "run")
  modeloutdir <- file.path(workflow_root, "out")
  dir.create(rundir, recursive = TRUE)
  dir.create(modeloutdir, recursive = TRUE)
  
  met_paths <- c("met_2010.nc", "met_2011.nc", "met_2012.nc")
  
  settings <- list(
    outdir = workflow_root,
    rundir = rundir,
    modeloutdir = modeloutdir,
    host = list(name = "localhost", rundir = rundir, outdir = modeloutdir),
    run = list(
      start.date = "2000-01-01",
      end.date = "2000-12-31",
      site = list(id = "1", name = "Test Site"),
      inputs = list(met = list(path = met_paths)),
      outdir = modeloutdir
    ),
    model = list(id = 99, type = "FAKE"),
    pfts = list(list(name = "pft1", posteriorid = NULL, constants = list())),
    sensitivity.analysis = list(ensemble.id = "SA-TEST"),
    workflow = list(id = 1),
    database = NULL,
    ensemble = list(
      samplingspace = list(
        parameters = list(method = "uniform"),
        met = list(method = "sampling")
      )
    )
  )
  
  # SA samples: 1 pft, 2 traits, 3 quantiles
  # expected runs: 1 median + 2 traits × 2 non-median = 5
  sa_samples <- list(
    pft1 = matrix(
      c(1, 2, 3, 4, 5, 6),
      nrow = 3, ncol = 2,
      dimnames = list(c("25", "50", "75"), c("Vcmax", "SLA"))
    )
  )
  
  # generate OAT design
  design_result <- generate_OAT_SA_design(settings, sa_samples = sa_samples)
  input_design <- design_result$X
  
  # call write.sa.configs with OAT design
  result <- PEcAn.uncertainty::write.sa.configs(
    defaults = settings$pfts,
    quantile.samples = sa_samples,
    settings = settings,
    model = "FAKE",
    write.to.db = FALSE,
    input_design = input_design
  )
  
  # verify write.sa.configs output structure (required for SA postprocessing)
  expect_true("runs" %in% names(result))
  expect_true("ensemble.id" %in% names(result))
  expect_true("pft1" %in% names(result$runs))
  
  # verify runs matrix structure matches sa_samples (required by run.sensitivity.analysis)
  runs_matrix <- result$runs$pft1
  expect_equal(rownames(runs_matrix), rownames(sa_samples$pft1))
  expect_equal(colnames(runs_matrix), colnames(sa_samples$pft1))
  
  # verify runs.txt created with correct count
  runs_file <- file.path(rundir, "runs.txt")
  expect_true(file.exists(runs_file))
  run_ids <- readLines(runs_file)
  expect_equal(length(run_ids), nrow(input_design))
  
  # verify run directories created (required for model output reading)
  for (run_id in run_ids) {
    expect_true(dir.exists(file.path(rundir, run_id)))
  }
})
