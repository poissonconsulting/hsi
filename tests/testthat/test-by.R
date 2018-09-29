context("by")

test_that("by", {
  expect_identical(hsi_by(c(1, 1.5, 2)), 0.5)
  expect_identical(hsi_by(2), 0)
  expect_error(hsi_by(numeric(0)), "x must have at least 1 element")
})
  