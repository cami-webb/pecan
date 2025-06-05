context("get.site.info")

test_that("get.site.info works with settings object", {
  # Create a simple settings object
  settings <- list(
    run = list(
      site = list(
        id = 1000000001,
        name = "Test Site",
        lat = 45.0,
        lon = -90.0
      )
    )
  )
  
  
  site_info <- get.site.info(settings)
  
  # Check the result
  expect_is(site_info, "data.frame")
  expect_equal(nrow(site_info), 1)
  expect_equal(site_info$site_id, 1000000001)
  expect_equal(site_info$site_name, "Test Site")
  expect_equal(site_info$lat, 45.0)
  expect_equal(site_info$lon, -90.0)
})

test_that("get.site.info works with CSV file", {
  # Create a temporary CSV file
  csv_file <- tempfile(fileext = ".csv")
  csv_data <- data.frame(
    site_id = c(123, 456),
    site_name = c("Site 1", "Site 2"),
    lat = c(40.0, 50.0),
    lon = c(-80.0, -100.0)
  )
  write.csv(csv_data, csv_file, row.names = FALSE)
  
  # Call get.site.info
  site_info <- get.site.info(csv_file)
  
  # Check the result
  expect_is(site_info, "data.frame")
  expect_equal(nrow(site_info), 2)
  expect_equal(site_info$site_id, c(123, 456))
  expect_equal(site_info$site_name, c("Site 1", "Site 2"))
  expect_equal(site_info$lat, c(40.0, 50.0))
  expect_equal(site_info$lon, c(-80.0, -100.0))
  
  # Clean up
  unlink(csv_file)
})

test_that("get.site.info works with MultiSettings object", {
  # Create a MultiSettings object
  settings1 <- list(
    run = list(
      site = list(
        id = 1000000004,
        name = "Multi Site 1",
        lat = 35.0,
        lon = -85.0
      )
    )
  )
  
  settings2 <- list(
    run = list(
      site = list(
        id = 1000000005,
        name = "Multi Site 2",
        lat = 55.0,
        lon = -95.0
      )
    )
  )
  
  multi_settings <- structure(
    list(settings1, settings2),
    class = "MultiSettings"
  )
  
  # Call get.site.info
  site_info <- get.site.info(multi_settings)
  
  # Check the result
  expect_is(site_info, "data.frame")
  expect_equal(nrow(site_info), 2)
  expect_equal(site_info$site_id, c(1000000004, 1000000005))
  expect_equal(site_info$site_name, c("Multi Site 1", "Multi Site 2"))
  expect_equal(site_info$lat, c(35.0, 55.0))
  expect_equal(site_info$lon, c(-85.0, -95.0))
})

test_that("get.site.info validates coordinates", {
  # Create a settings object with invalid coordinates
  settings <- list(
    run = list(
      site = list(
        id = 999,
        name = "Invalid Site",
        lat = 100.0,  # Invalid latitude
        lon = -90.0
      )
    )
  )
  
  # Should throw error with validation
  expect_error(get.site.info(settings, validate = TRUE))
  
  # Should work without validation
  site_info <- get.site.info(settings, validate = FALSE)
  expect_is(site_info, "data.frame")
  expect_equal(site_info$lat, 100.0)
})
