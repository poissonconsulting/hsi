context("check")

test_that("check", {
  expect_identical(check_hsi(hsi_data), hsi_data)
  expect_error(check_hsi(hsi_data, by = 2), 
                   "column 'Habitat' of hsi_data increments must be 2 not 1")
})