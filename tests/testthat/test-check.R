context("check")

test_that("check_hsi", {
  expect_identical(check_hsi(hsi_data), hsi_data)
})

test_that("check_transect", {
  expect_identical(check_transect(trans_data), trans_data)
})
