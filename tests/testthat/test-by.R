test_that("by", {
  expect_identical(hsi_by(c(1, 1.5, 2)), 0.5)
  expect_identical(hsi_by(2), 0)
  expect_error(hsi_by(numeric(0)), "`length(x)` must be greater than 0, not 0.", fixed = TRUE)
})

test_that("seq_by",{
  expect_identical(hsi_seq_by(1, by = 1), c(0, 1, 2, 3))
  expect_identical(hsi_seq_by(c(1,2,1.5), by = 0.5), c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0))
  expect_identical(hsi_seq_by(c(1,2,1.5), by = 1), c(0, 1, 2, 3, 4))
  expect_identical(hsi_seq_by(c(1,2,1.5), by = 2), c(-2, 0, 2, 4, 6))
  expect_identical(hsi_seq_by(c(1,2,1.5), by = 3), c(-3, 0, 3, 6))
  
  expect_identical(hsi_seq_by(c(1.01,1.99), by = 1), c(0,1,2,3))
  expect_identical(hsi_seq_by(c(1,2), by = 1), c(0,1,2,3,4))
  expect_identical(hsi_seq_by(c(0.9,2), by = 1), c(-1,0,1,2,3,4))
  expect_identical(hsi_seq_by(c(0.9,2,2.9), by = 1), c(-1,0,1,2,3,4))
  expect_identical(hsi_seq_by(c(0.1,2,2.9), by = 1), c(-1,0,1,2,3,4))
  expect_identical(hsi_seq_by(c(0,2,2.9), by = 1), c(-1,0,1,2,3,4))
  expect_identical(hsi_seq_by(c(10.11,9.85), by = 1), c(8,9,10,11,12))
})

test_that("set_by",{
  expect_identical(hsi_set_by(hsi_data), hsi_data)
  expect_identical(hsi_set_by(hsi_data, by = 2), 
                   tibble::tibble(Habitat = c(0,2,4,6,8,10),
                                  Index = c(0,1, 0.5, 0.4, 0.2, 0)))
  expect_identical(hsi_set_by(hsi_data, by = 2.5), 
                   tibble::tibble(Habitat = c(0,2.5,5,7.5,10),
                                  Index = c(0,1, 0.5, 0.25, 0)))
  x <- hsi_set_by(hsi_data, by = 0.1)
  expect_identical(nrow(x), 92L)
  expect_equal(x$Habitat[1:3], c(0.9, 1, 1.1))
  expect_equal(x$Index[1:3], c(0,0,0.1))
  expect_identical(x$Habitat[90:91], c(9.8,9.9))
  expect_equal(x$Index[90:91], c(0.02,0.01))
})
