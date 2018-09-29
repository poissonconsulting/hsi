context("utils")

test_that("seq_by",{
  expect_identical(seq_by(c(1,2), by = 1), c(0,1,2,3))
  expect_identical(seq_by(c(0.9,2), by = 1), c(-1,0,1,2,3))
  expect_identical(seq_by(c(0.9,2,2.9), by = 1), c(-1,0,1,2,3,4))
  expect_identical(seq_by(c(0.1,2,2.9), by = 1), c(-1,0,1,2,3,4))
  expect_identical(seq_by(c(0,2,2.9), by = 1), c(-1,0,1,2,3,4))
})
