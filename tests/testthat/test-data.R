context("data")

test_that("hsi_data", {
  expect_identical(check_hsi(hsi_data), hsi_data)
})

test_that("trans_data", {
  expect_identical(check_transect(trans_data), trans_data)
})
