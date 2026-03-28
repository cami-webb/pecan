testthat::test_that("SOC gain is returned as negative CO2e", {
  testthat::expect_equal(co2e(delta_soc = 1), -(44 / 12))
})

test_that("CH4 conversion uses expected GWP100 values for specified gwp version", {
  expect_equal(co2e(ch4 = 1, gwp = "AR4"), 25)
  expect_equal(co2e(ch4 = 1, gwp = "AR5"), 28)
  expect_equal(co2e(ch4 = 1, gwp = "AR6"), 29.8)
})

test_that("N2O conversion uses expected GWP100 values for specified gwp version", {
  expect_equal(co2e(n2o = 1, gwp = "AR4"), 298)
  expect_equal(co2e(n2o = 1, gwp = "AR5"), 265)
  expect_equal(co2e(n2o = 1, gwp = "AR6"), 273)
})

test_that("co2e function correctly sums across delta SOC, N2O, CH4", {
  delta_soc_co2e <- co2e(delta_soc = 1)
  n2o_co2e <- co2e(n2o = 1)
  ch4_co2e <- co2e(ch4 = 1)
  all_co2e <- co2e(delta_soc = 1, ch4 = 1, n2o = 1)
  expect_equal(all_co2e, sum(delta_soc_co2e, n2o_co2e, ch4_co2e))
})

testthat::test_that("unsupported GWP values error", {
  testthat::expect_error(co2e(gwp = "BAD"), "AR4.*AR5.*AR6")
})
