# Tests for get.trait.data.pft()
# Package: PEcAn.DB
#
# Purpose: Lock down the current behavior of get.trait.data.pft() before
# refactoring. These act as a regression safety net — if refactoring changes
# any observable behavior, these tests will catch it.
#
# Tests requiring a live BETYdb connection skip cleanly when unavailable.
# ── Fixtures ──────────────────────────────────────────────────────────────────
# Helper needed for the restored cultivar test
dbdir <- file.path(tempdir(), "dbfiles")

get_pft <- function(pftname) {
  get.trait.data.pft(
    pft         = list(name = pftname, outdir = withr::local_tempdir()),
    trait.names = "SLA",
    dbfiles     = dbdir,
    modeltype   = NULL,
    dbcon       = test_dbcon)
}

make_test_pft <- function(outdir) {
  list(
    name        = "temperate.Early_Hardwood",
    outdir      = outdir,
    posteriorid = NULL,
    constants   = list()
  )
}

make_empty_pft <- function(outdir) {
  # PFT that exists in BETY but has no observations for the requested traits
  list(
    name        = "soil.ALL",
    outdir      = outdir,
    posteriorid = NULL,
    constants   = list()
  )
}

std_modeltype <- "SIPNET"
std_traits    <- c("SLA", "Vcmax", "leaf_respiration_rate_m2")

skip_if_no_db <- function(dbcon) {
  if (is.null(dbcon) || inherits(dbcon, "try-error")) {
    skip("No test database connection available — skipping live DB test")
  }
  # Also skip if BETYdb schema hasn't been loaded (empty database)
  has_schema <- tryCatch({
    DBI::dbExistsTable(dbcon, "pfts")
  }, error = function(e) FALSE)
  if (!has_schema) {
    skip("No test database connection available — skipping live DB test")
  }
}

# Try to open a DB connection — tests skip gracefully if unavailable
test_dbcon <- tryCatch(
  PEcAn.DB::db.open(
    params = list(
      driver   = "PostgreSQL",
      user     = Sys.getenv("PECAN_TEST_DB_USER", "bety"),
      password = Sys.getenv("PECAN_TEST_DB_PASS", "bety"),
      host     = Sys.getenv("PECAN_TEST_DB_HOST", "localhost"),
      dbname   = Sys.getenv("PECAN_TEST_DB_NAME", "bety"),
      port     = as.integer(Sys.getenv("PECAN_TEST_DB_PORT", "5432"))
    )
  ),
  error = function(e) NULL
)

test_that("get.trait.data.pft fails gracefully with NULL dbcon", {
  outdir <- withr::local_tempdir()
  
  expect_error(
    get.trait.data.pft(
      pft         = make_test_pft(outdir),
      modeltype   = std_modeltype,
      dbfiles     = outdir,
      dbcon       = NULL,
      trait.names = std_traits
    )
  )
})

test_that("get.trait.data.pft fails with invalid modeltype and NULL dbcon", {
  outdir <- withr::local_tempdir()
  
  expect_error(
    get.trait.data.pft(
      pft         = make_test_pft(outdir),
      modeltype   = "NONEXISTENT_MODEL",
      dbfiles     = outdir,
      dbcon       = NULL,
      trait.names = std_traits
    )
  )
})

test_that("get.trait.data.pft errors with no arguments", {
  expect_error(get.trait.data.pft())
})


# ── Block 1: Files are written to disk ────────────────────────────────────────

test_that("get.trait.data.pft() writes trait.data.Rdata to pft$outdir", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  get.trait.data.pft(
    pft         = make_test_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

  expect_true(file.exists(file.path(outdir, "trait.data.Rdata")))
})

test_that("get.trait.data.pft() writes prior.distns.Rdata to pft$outdir", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  get.trait.data.pft(
    pft         = make_test_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

  expect_true(file.exists(file.path(outdir, "prior.distns.Rdata")))
})

test_that("get.trait.data.pft() writes both expected files in one call", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  get.trait.data.pft(
    pft         = make_test_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

  rdata_files <- list.files(outdir, pattern = "\\.Rdata$")
  expect_true("trait.data.Rdata"   %in% rdata_files)
  expect_true("prior.distns.Rdata" %in% rdata_files)
})


# ── Block 2: Return value shape ───────────────────────────────────────────────

test_that("get.trait.data.pft() returns a list with field 'name'", {
  skip_if_no_db(test_dbcon)
  outdir   <- withr::local_tempdir()
  test_pft <- make_test_pft(outdir)

  result <- get.trait.data.pft(
    pft         = test_pft,
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

  expect_true("name" %in% names(result))
  expect_equal(result$name, test_pft$name)
})

test_that("get.trait.data.pft() returns a list with field 'posteriorid'", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  result <- get.trait.data.pft(
    pft         = make_test_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

  expect_true("posteriorid" %in% names(result))
})

test_that("get.trait.data.pft() returns a list with field 'outdir'", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  result <- get.trait.data.pft(
    pft         = make_test_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

  expect_true("outdir" %in% names(result))
  expect_equal(result$outdir, outdir)
})

test_that("CURRENT BEHAVIOR: get.trait.data.pft() discards computed data", {
  # NOTE: This test documents the CURRENT behavior where trait_data and
  # prior_distns are saved to disk but NOT returned to the caller.
  # After the modularity refactor, this test should be UPDATED (not deleted)
  # to verify that the new pure function DOES return these objects.

  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  result <- get.trait.data.pft(
    pft         = make_test_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

  expect_false("trait_data"   %in% names(result))
  expect_false("prior_distns" %in% names(result))
})


# ── Block 3: Contents of written files ───────────────────────────────────────

test_that("trait.data.Rdata contains a named list", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  get.trait.data.pft(
    pft         = make_test_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

  env <- new.env(parent = emptyenv())
  load(file.path(outdir, "trait.data.Rdata"), envir = env)

  expect_true(exists("trait.data", envir = env))
  expect_true(is.list(env$trait.data))
})

test_that("prior.distns.Rdata contains a data frame with expected columns", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  get.trait.data.pft(
    pft         = make_test_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

  env <- new.env(parent = emptyenv())
  load(file.path(outdir, "prior.distns.Rdata"), envir = env)

  expect_true(exists("prior.distns", envir = env))
  expect_true(is.data.frame(env$prior.distns))

  missing_cols <- setdiff(c("distn", "parama", "paramb"), colnames(env$prior.distns))
  expect_equal(length(missing_cols), 0L,
    label = paste("prior.distns missing columns:", paste(missing_cols, collapse = ", ")))
})


# ── Block 4: PFT with no observations ────────────────────────────────────────

test_that("get.trait.data.pft() does not error for a PFT with no observations", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()
  
  soil_exists <- tryCatch({
    nrow(DBI::dbGetQuery(test_dbcon, 
      "SELECT 1 FROM pfts WHERE name = 'soil.ALL' LIMIT 1")) > 0
  }, error = function(e) FALSE)
  skip_if_not(soil_exists, "soil.ALL PFT not present in test BETY")
  
  result <- get.trait.data.pft(
    pft         = make_empty_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )
  
  # Clean up any DB records created during test
  if (!is.null(result$posteriorid)) {
    posteriorid <- result$posteriorid
    withr::defer({
      try(DBI::dbExecute(test_dbcon,
        paste0("DELETE FROM dbfiles WHERE container_type = 'Posterior' AND container_id = ",
               posteriorid)), silent = TRUE)
      try(DBI::dbExecute(test_dbcon,
        paste0("DELETE FROM posteriors WHERE id = ",
               posteriorid)), silent = TRUE)
    })
  }
  
  expect_true(is.list(result))
})

test_that("get.trait.data.pft() writes prior.distns.Rdata even when trait data is empty", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  soil_exists <- tryCatch({
    nrow(DBI::dbGetQuery(test_dbcon, 
      "SELECT 1 FROM pfts WHERE name = 'soil.ALL' LIMIT 1")) > 0
  }, error = function(e) FALSE)
  skip_if_not(soil_exists, "soil.ALL PFT not present in test BETY")

  result <- get.trait.data.pft(
    pft         = make_empty_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

    # Clean up any DB records created during test
  if (!is.null(result$posteriorid)) {
    posteriorid <- result$posteriorid
    withr::defer({
      try(DBI::dbExecute(test_dbcon,
        paste0("DELETE FROM dbfiles WHERE container_type = 'Posterior' AND container_id = ",
               posteriorid)), silent = TRUE)
      try(DBI::dbExecute(test_dbcon,
        paste0("DELETE FROM posteriors WHERE id = ",
               posteriorid)), silent = TRUE)
    })
  }

  expect_true(file.exists(file.path(outdir, "prior.distns.Rdata")))
})


# ── Block 5: Caching behavior ─────────────────────────────────────────────────

test_that("second call uses cache — files are not rewritten", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  args <- list(
    pft         = make_test_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

  # First call — creates files
  do.call(get.trait.data.pft, args)
  
  trait_file <- file.path(outdir, "trait.data.Rdata")
  prior_file <- file.path(outdir, "prior.distns.Rdata")
  
  expect_true(file.exists(trait_file))
  expect_true(file.exists(prior_file))
  
  # Record file sizes (not timestamps — more reliable)
  size_before_trait <- file.size(trait_file)
  size_before_prior <- file.size(prior_file)
  
  # Second call — should use cache
  result2 <- do.call(get.trait.data.pft, args)
  
  # Files should still exist and be the same size
  expect_equal(file.size(trait_file), size_before_trait)
  expect_equal(file.size(prior_file), size_before_prior)
  
  # Return value should still be valid
  expect_true("name" %in% names(result2))
})

test_that("second call still returns a valid pft list when using cache", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  args <- list(
    pft         = make_test_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

  do.call(get.trait.data.pft, args)
  result <- do.call(get.trait.data.pft, args)

  expect_true("name"        %in% names(result))
  expect_true("posteriorid" %in% names(result))
  expect_true("outdir"      %in% names(result))
})


# ── Block 6: End-to-end check ─────────────────────────────────────────────────
# This test requires a real DB connection to verify the full
# query → save → return cycle works correctly.

test_that("end-to-end: written files are consistent with returned pft", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  result <- get.trait.data.pft(
    pft         = make_test_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

  # Load what was written to disk
  trait_env <- new.env(parent = emptyenv())
  prior_env <- new.env(parent = emptyenv())
  load(file.path(outdir, "trait.data.Rdata"), envir = trait_env)
  load(file.path(outdir, "prior.distns.Rdata"), envir = prior_env)

  # Verify files contain actual data structures
  expect_true(is.list(trait_env$trait.data))
  expect_true(is.data.frame(prior_env$prior.distns))

  # Verify return value is consistent
  expect_equal(result$name, "temperate.Early_Hardwood")
  expect_equal(result$outdir, outdir)
})

# ── Block 7: Error cases ─────────────────────────────────────────────────────

test_that("get.trait.data.pft() errors for non-existent PFT name", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  expect_error(
    get.trait.data.pft(
      pft       = list(name = "NOTAPFT", outdir = outdir,
                        posteriorid = NULL, constants = list()),
      modeltype = std_modeltype,
      dbfiles   = outdir,
      dbcon     = test_dbcon,
      trait.names = std_traits
    ),
    "Could not find pft"
  )
})

test_that("get.trait.data.pft() errors when multiple PFTs share a name", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  # "soil" matches multiple PFTs in standard BETY
  multi_exists <- tryCatch({
    n <- DBI::dbGetQuery(test_dbcon, "SELECT count(*) AS n FROM pfts WHERE name = 'soil'")$n
    n > 1
  }, error = function(e) FALSE)
  skip_if_not(multi_exists, "Need multiple PFTs named 'soil' to test this case")

  expect_error(
    get.trait.data.pft(
      pft       = list(name = "soil", outdir = outdir,
                        posteriorid = NULL, constants = list()),
      modeltype = NULL,
      dbfiles   = outdir,
      dbcon     = test_dbcon,
      trait.names = "SLA"
    ),
    "Multiple PFTs"
  )
})

# ── Restored: Skipped cultivar test (see #1958) ──────────────────────────────
test_that("reference species and cultivar PFTs write traits properly", {
  skip("Disabled until Travis bety contains Pavi_alamo and Pavi_all (#1958)")

  pavi_sp <- get_pft("pavi")
  expect_equal(pavi_sp$name, "pavi")
  sp_csv <- file.path(dbdir, "posterior", pavi_sp$posteriorid, "species.csv")
  sp_trt <- file.path(dbdir, "posterior", pavi_sp$posteriorid, "trait.data.csv")
  expect_true(file.exists(sp_csv))
  expect_true(file.exists(sp_trt))
  expect_gt(file.info(sp_csv)$size, 40)
  expect_gt(file.info(sp_trt)$size, 215)

  pavi_cv <- get_pft("Pavi_alamo")
  expect_equal(pavi_cv$name, "Pavi_alamo")
  cv_csv <- file.path(dbdir, "posterior", pavi_cv$posteriorid, "cultivars.csv")
  cv_trt <- file.path(dbdir, "posterior", pavi_cv$posteriorid, "trait.data.csv")
  expect_true(file.exists(cv_csv))
  expect_true(file.exists(cv_trt))
  expect_gt(file.info(cv_csv)$size, 63)
  expect_gt(file.info(cv_trt)$size, 215)

  pavi_allcv <- get_pft("Pavi_all")
  expect_equal(pavi_allcv$name, "Pavi_all")
  allcv_csv <- file.path(dbdir, "posterior", pavi_allcv$posteriorid, "cultivars.csv")
  allcv_trt <- file.path(dbdir, "posterior", pavi_allcv$posteriorid, "trait.data.csv")
  expect_true(file.exists(allcv_csv))
  expect_true(file.exists(allcv_trt))
  expect_gt(file.info(allcv_csv)$size, 63)
  expect_gt(file.info(allcv_trt)$size, 215)

  expect_gt(file.info(allcv_csv)$size, file.info(cv_csv)$size)
  expect_gt(file.info(allcv_trt)$size, file.info(cv_trt)$size)
})

if (!is.null(test_dbcon)) {
  withr::defer(
    try(PEcAn.DB::db.close(test_dbcon), silent = TRUE),
    envir = testthat::teardown_env()
  )
}