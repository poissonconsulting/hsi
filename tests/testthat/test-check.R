context("check")

test_that("check_hsi", {
  expect_identical(check_hsi(hsi_data), hsi_data)
  expect_error(check_hsi(hsi_data, hsi_multi = 2), 
                   "hsi multiplier of 'hsi_data' must be 2 not 1")
  x2 <- hsi_data
  x2$Habitat[1] <- x2$Habitat[1] - 1L
  expect_error(check_hsi(x2), "column 'Habitat' of x2 must be consecutive values")
})

test_that("check_transect", {
  expect_identical(check_transect(trans_data), trans_data)
})
