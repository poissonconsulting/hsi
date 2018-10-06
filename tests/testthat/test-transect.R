context("transect")

test_that("hsi_transect_to_sample", {
  x <- hsi_transect_to_sample(trans_data, n = 1000L)
  expect_identical(length(x), 1000L)
  expect_equal(x[1:5], c(10.0000, 10.0005, 10.0010, 10.0015, 10.0020),
                   tolerance = 1e-06)
  
  expect_identical(hsi_transect_to_sample(trans_data,n=6), 
                   c(10.000, 10.100, 10.150, 10.000,  9.925, 9.850))
})

test_that("hsi_transect_to_index", {
  x <- hsi_transect_to_index(trans_data)
  expect_identical(colnames(x), c("Habitat", "Index"))
  expect_equal(x$Habitat, 8:11)
  expect_equal(x$Index, c(0, 0.66666667, 1, 0))
  
  data <- data.frame(Distance = c(0,1), Habitat = c(1,2))
  x <- hsi_transect_to_index(data)
  expect_equal(x$Index, c(0, 1, 1.000001e-06, 0), tolerance = 1e-06)
})

test_that("hsi_transect_set_by", {
  x <- hsi_transect_set_by(trans_data)
  expect_identical(x$Distance[1:2], c(1, 1.5))
  expect_identical(x$Habitat[1:2], c(10, 10.05))
  expect_identical(hsi_transect_set_by(x, by = 1), 
                   hsi_transect_set_by(trans_data, by = 1))
})
