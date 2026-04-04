context("ndti_to_sipnet_tillage")

# ---- helpers ----
make_dates <- function(n) as.Date("2022-01-01") + seq(0, n - 1)

# ---- structure ----
testthat::test_that("returns a data.frame with correct columns and types", {
  result <- ndti_to_sipnet_tillage(make_dates(2), c(0.2, 0.5))
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_named(result, c("date", "tillage_eff_0to1"))
  testthat::expect_s3_class(result$date, "Date")
  testthat::expect_type(result$tillage_eff_0to1, "double")
  testthat::expect_equal(nrow(result), 2)
})

# ---- boundary values ----
testthat::test_that("delta_ndti at or below no_till_threshold gives zero", {
  result <- ndti_to_sipnet_tillage(make_dates(3), c(0.0, 0.15, 0.30))
  testthat::expect_equal(result$tillage_eff_0to1, c(0, 0, 0))
})

testthat::test_that("delta_ndti at or above conv_till_threshold gives max_modifier", {
  result <- ndti_to_sipnet_tillage(make_dates(3), c(0.70, 0.85, 1.00))
  testthat::expect_equal(result$tillage_eff_0to1, c(1, 1, 1))
})

testthat::test_that("midpoint maps to half of max_modifier with linear method", {
  mid <- (0.30 + 0.70) / 2  # 0.50
  result <- ndti_to_sipnet_tillage(make_dates(1), mid, method = "linear")
  testthat::expect_equal(result$tillage_eff_0to1, 0.5, tolerance = 1e-9)
})

# ---- max_modifier ----
testthat::test_that("max_modifier scales output correctly", {
  result <- ndti_to_sipnet_tillage(
    make_dates(2), c(0.30, 0.70), max_modifier = 0.5
  )
  testthat::expect_equal(result$tillage_eff_0to1, c(0, 0.5))
})

# ---- method = saturating ----
testthat::test_that("saturating method is monotone and bounded", {
  deltas <- seq(0, 1, length.out = 50)
  result <- ndti_to_sipnet_tillage(make_dates(50), deltas, method = "saturating")
  testthat::expect_true(all(result$tillage_eff_0to1 >= 0))
  testthat::expect_true(all(result$tillage_eff_0to1 <= 1))
  testthat::expect_true(all(diff(result$tillage_eff_0to1) >= -1e-10))
})

testthat::test_that("saturating method gives zero at no_till_threshold", {
  result <- ndti_to_sipnet_tillage(make_dates(1), 0.30, method = "saturating")
  testthat::expect_equal(result$tillage_eff_0to1, 0)
})

# ---- custom thresholds ----
testthat::test_that("custom thresholds are respected", {
  result <- ndti_to_sipnet_tillage(
    make_dates(3),
    c(0.10, 0.15, 0.20),
    no_till_threshold   = 0.10,
    conv_till_threshold = 0.20
  )
  testthat::expect_equal(result$tillage_eff_0to1[1], 0)
  testthat::expect_equal(result$tillage_eff_0to1[3], 1)
  testthat::expect_equal(result$tillage_eff_0to1[2], 0.5, tolerance = 1e-9)
})

# ---- NA handling ----
testthat::test_that("NA in delta_ndti propagates to output", {
  result <- ndti_to_sipnet_tillage(make_dates(3), c(0.5, NA, 0.8))
  testthat::expect_false(is.na(result$tillage_eff_0to1[1]))
  testthat::expect_true(is.na(result$tillage_eff_0to1[2]))
  testthat::expect_false(is.na(result$tillage_eff_0to1[3]))
})

testthat::test_that("negative delta_ndti is clamped to zero", {
  # PEcAn.logger::logger.warn does not produce an R warning,
  # so we test the output value directly
  result <- ndti_to_sipnet_tillage(make_dates(2), c(-0.1, 0.5))
  # negative input gets clamped at no_till_threshold, maps to 0 
  testthat::expect_equal(result$tillage_eff_0to1[1], 0)
   testthat::expect_equal(result$tillage_eff_0to1[2], 0.5)
  testthat::expect_equal(nrow(result), 2)
})

# ---- output fits schema ----
testthat::test_that("output tillage_eff_0to1 satisfies schema minimum of 0", {
  deltas <- seq(0, 1, length.out = 20)
  result <- ndti_to_sipnet_tillage(make_dates(20), deltas)
  testthat::expect_true(all(result$tillage_eff_0to1 >= 0, na.rm = TRUE))
})

# ---- error conditions ----
testthat::test_that("mismatched lengths cause error", {
  testthat::expect_error(
    ndti_to_sipnet_tillage(make_dates(2), c(0.1, 0.2, 0.3)),
    "same length"
  )
})

testthat::test_that("inverted thresholds cause error", {
  testthat::expect_error(
    ndti_to_sipnet_tillage(make_dates(1), 0.5,
      no_till_threshold   = 0.70,
      conv_till_threshold = 0.30
    ),
    "must be less than"
  )
})

testthat::test_that("negative max_modifier causes error", {
  testthat::expect_error(
    ndti_to_sipnet_tillage(make_dates(1), 0.5, max_modifier = -0.1),
    "non-negative"
  )
})

testthat::test_that("single observation works", {
  result <- ndti_to_sipnet_tillage(as.Date("2022-06-15"), 0.50)
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$tillage_eff_0to1, 0.5, tolerance = 1e-9)
})