context("sample")

test_that("sample_to_index", {
  expect_equal(hsi_sample_to_index(c(1.01, 1.99), by = 1), 
               tibble::tibble(Habitat = c(0,1,2), Index = c(0, 1, 0)))
  expect_equal(hsi_sample_to_index(c(1, 2), by = 1), 
               tibble::tibble(Habitat = c(0,1,2,3), Index = c(0, 1, 1, 0)))
  expect_equal(hsi_sample_to_index(c(0.9, 2), by = 1), 
               tibble::tibble(Habitat = c(-1,0,1,2,3), Index = c(0, 1, 0, 1, 0)))
  expect_equal(hsi_sample_to_index(c(1), by = 1), 
               tibble::tibble(Habitat = c(-0,1,2), Index = c(0, 1, 0)))
  expect_equal(hsi_sample_to_index(c(10.11,9.85), by = 1), 
               tibble::tibble(Habitat = c(8,9,10,11), Index = c(0, 1, 1, 0)))
})
