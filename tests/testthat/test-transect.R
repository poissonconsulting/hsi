context("transect")

test_that("hsi_transect_to_sample", {
  x <- hsi_transect_to_sample(trans_data)
  expect_identical(length(x), 1000L)
  expect_equal(x[1:5], c(10.0000, 10.0005, 10.0010, 10.0015, 10.0020),
                   tolerance = 1e-06)
  
  expect_identical(hsi_transect_to_sample(trans_data,n=6), 
                   c(10.000, 10.100, 10.150, 10.000,  9.925, 9.850))
})

test_that("hsi_transect_to_index", {
  x <- hsi_transect_to_index(trans_data)
  expect_identical(colnames(x), c("Habitat", "Index"))
  expect_equal(x$Habitat, seq(9.8, 10.15, by = 0.05))
  expect_equal(x$Index[1:2], c(0.003745318, 0.498127341))
  
  data <- data.frame(Distance = c(0,1), Habitat = c(1,2))
  x <- hsi_transect_to_index(data)
  expect_equal(x$Index, c(0, 1, 0.001001001), tolerance = 1e-06)
})
  