# Tests for get.trait.data.pft()
# Package: PEcAn.DB
#
# Purpose: Lock down the current behavior of get.trait.data.pft() before
# refactoring. These act as a regression safety net — if refactoring changes
# any observable behavior, these tests will catch it.
#
# Tests requiring a live BETYdb connection skip cleanly when unavailable.
# ── Fixtures ──────────────────────────────────────────────────────────────────

make_test_pft <- function(outdir) {
  list(
    name        = "temperate.Early_Hardwood",
    outdir      = outdir,
    posteriorid = NULL,
    constants   = list()
  )
}

make_empty_pft <- function(outdir) {
  # Uses a deliberately nonexistent PFT name to test empty-data behavior
  list(
    name        = "test.EmptyPFT.NoSpecies",
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

# ── Block 0: Tests that work WITHOUT a database ──────────────────────────────

test_that("make_test_pft returns correctly structured list", {
  outdir <- withr::local_tempdir()
  pft <- make_test_pft(outdir)
  
  expect_true(is.list(pft))
  expect_equal(pft$name, "temperate.Early_Hardwood")
  expect_equal(pft$outdir, outdir)
  expect_null(pft$posteriorid)
  expect_true(is.list(pft$constants))
})

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

test_that("get.trait.data.pft accepts trait.names parameter", {
  expect_true(
    "trait.names" %in% names(formals(get.trait.data.pft))
  )
})

test_that("make_empty_pft returns correctly structured list", {
  outdir <- withr::local_tempdir()
  pft <- make_empty_pft(outdir)
  
  expect_true(is.list(pft))
  expect_equal(pft$name, "test.EmptyPFT.NoSpecies")
  expect_equal(pft$outdir, outdir)
  expect_null(pft$posteriorid)
  expect_true(is.list(pft$constants))
})

test_that("get.trait.data.pft function exists and is callable", {
  expect_true(is.function(get.trait.data.pft))
})

test_that("get.trait.data.pft has expected parameters", {
  params <- names(formals(get.trait.data.pft))
  
  expect_true("pft"         %in% params)
  expect_true("modeltype"   %in% params)
  expect_true("dbfiles"     %in% params)
  expect_true("dbcon"       %in% params)
  expect_true("trait.names" %in% params)
})

test_that("get.trait.data.pft errors with no arguments", {
  expect_error(get.trait.data.pft())
})

test_that("std_traits contains expected trait names", {
  expect_true("SLA"    %in% std_traits)
  expect_true("Vcmax"  %in% std_traits)
  expect_equal(length(std_traits), 3)
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

  expect_no_error(
    get.trait.data.pft(
      pft         = make_empty_pft(outdir),
      modeltype   = std_modeltype,
      dbfiles     = outdir,
      dbcon       = test_dbcon,
      trait.names = std_traits
    )
  )
})

test_that("get.trait.data.pft() writes prior.distns.Rdata even when trait data is empty", {
  skip_if_no_db(test_dbcon)
  outdir <- withr::local_tempdir()

  get.trait.data.pft(
    pft         = make_empty_pft(outdir),
    modeltype   = std_modeltype,
    dbfiles     = outdir,
    dbcon       = test_dbcon,
    trait.names = std_traits
  )

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

if (!is.null(test_dbcon)) {
  if (exists("teardown_env", where = asNamespace("testthat"))) {
    withr::defer(
      try(PEcAn.DB::db.close(test_dbcon), silent = TRUE),
      envir = testthat::teardown_env()
    )
  } else {
    # Fallback for testthat < 3.0.0
    on.exit(try(PEcAn.DB::db.close(test_dbcon), silent = TRUE), add = TRUE)
  }
}