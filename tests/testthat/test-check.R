context("check")

test_that("check", {
  expect_identical(check_hsi(hsi_data), hsi_data)
  expect_error(check_hsi(hsi_data, by = 2), 
                   "column 'Habitat' of hsi_data increments must be 2 not 1")
  x2 <- hsi_data
  x2$Habitat <- x2$Habitat + 0.1
  expect_error(check_hsi(x2), "column 'Habitat' of x2 must have 2 in its sequence")
  expect_identical(check_hsi(x2, by = NULL), x2)
})