context("utils")

test_that("by", {
  expect_identical(hsi_by(c(1, 1.5, 2)), 0.5)
  expect_identical(hsi_by(2), 0)
  expect_error(hsi_by(numeric(0)), "x must have at least 1 element")
})

test_that("seq_by",{
  expect_identical(hsi_seq_by(c(1,2,1.5)), c(0.5, 1.0, 1.5, 2.0, 2.5))
  expect_identical(hsi_seq_by(c(1,2,1.5), by = 1), c(0,1,2,3))
  expect_identical(hsi_seq_by(c(1,2,1.5), by = 2), c(0, 2, 4))
  expect_identical(hsi_seq_by(c(1,2,1.5), by = 3), c(0, 3))

  expect_identical(hsi_seq_by(c(1,2)), c(0,1,2,3))
  expect_identical(hsi_seq_by(c(0.9,2), by = 1), c(0,1,2,3))
  expect_identical(hsi_seq_by(c(0.9,2,2.9), by = 1), c(0,1,2,3))
  expect_identical(hsi_seq_by(c(0.1,2,2.9), by = 1), c(0,1,2,3))
  expect_identical(hsi_seq_by(c(0,2,2.9), by = 1), c(-1,0,1,2,3))
  
})
