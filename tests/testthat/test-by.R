context("by")

test_that("by", {
  expect_identical(hsi_by(c(1, 1.5, 2)), 0.5)
  expect_identical(hsi_by(2), 0)
  expect_error(hsi_by(c(2, 1.5, 1)), "x must be sorted")
})
  