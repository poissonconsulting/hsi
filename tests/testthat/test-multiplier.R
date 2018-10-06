context("multiplier")

test_that("multiplier", {
  x <- 2
  expect_identical(hsi_multiplier(x), 1)
  attr(x, "hsi_multi") <- 3
  expect_identical(hsi_multiplier(x), 3)
})
