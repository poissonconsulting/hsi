context("sample")

test_that("sample_to_index", {
  expect_identical(hsi_sample_to_index(c(1, 1.5, 2)),
                   tibble::tibble(Habitat = c(1, 1.5, 2), Index = rep(1, 3)))
  expect_identical(hsi_sample_to_index(c(1, 1.5, 2), by = 1),
                   tibble::tibble(Habitat = c(1, 2), Index = c(1, 0.5)))
  expect_error(hsi_sample_to_index(1), "the values in by must lie between 1e-04 and 1000")
  expect_error(hsi_sample_to_index(c(1,1)), "the values in by must lie between 1e-04 and 1000")
})
  