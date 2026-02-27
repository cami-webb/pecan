test_that("extract_chirps_remote returns data for single date", {
  skip_if_offline()

  pts <- tibble::tibble(lon = c(-120, -110), lat = c(35, 40), site_id = 1:2)

  result <- extract_chirps_remote(pts, as.Date("2020-06-15"))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("date" %in% names(result))
  expect_true("precip_mm_day" %in% names(result))
  expect_true(all(result$date == as.Date("2020-06-15")))
  expect_type(result$precip_mm_day, "double")
  expect_true(all(result$precip_mm_day >= 0, na.rm = TRUE))
})

test_that("extract_chirps_remote handles multiple dates in same year", {
  skip_if_offline()

  pts <- tibble::tibble(lon = c(-120, -110), lat = c(35, 40), site_id = 1:2)
  dates <- seq(as.Date("2020-06-01"), as.Date("2020-06-30"), by = "1 day")

  result <- extract_chirps_remote(pts, dates)

  expect_equal(nrow(result), 2 * 30)
  expect_equal(unique(lubridate::year(result$date)), 2020)
  expect_equal(sort(unique(result$date)), sort(dates))
  expect_type(result$precip_mm_day, "double")
  expect_true(all(result$precip_mm_day >= 0, na.rm = TRUE))
})

test_that("extract_chirps_remote handles dates spanning multiple years", {
  skip_if_offline()

  pts <- tibble::tibble(lon = c(-120, -110), lat = c(35, 40), site_id = 1:2)
  dates <- c(
    as.Date("2020-12-15"),
    as.Date("2020-12-31"),
    as.Date("2021-01-01"),
    as.Date("2021-01-15")
  )

  result <- extract_chirps_remote(pts, dates)

  expect_equal(nrow(result), 2 * 4)
  expect_equal(sort(unique(result$date)), sort(dates))
  expect_true(all(result$precip_mm_day >= 0, na.rm = TRUE))
})

test_that("extract_chirps_remote output has correct structure", {
  skip_if_offline()

  pts <- tibble::tibble(lon = c(-120, -110), lat = c(35, 40), site_id = 1:2)
  dates <- seq(as.Date("2020-06-01"), as.Date("2020-06-10"), by = "1 day")

  result <- extract_chirps_remote(pts, dates)

  expect_equal(nrow(result), nrow(pts) * length(dates))
  expect_true("date" %in% names(result))
  expect_true("precip_mm_day" %in% names(result))
  expect_true(all(result$date %in% dates))
  expect_true(is.numeric(result$precip_mm_day))
  expect_true(all(is.na(result$precip_mm_day) | result$precip_mm_day >= 0))
})
