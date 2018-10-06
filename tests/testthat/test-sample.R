context("sample")

test_that("sample_to_index", {
  expect_identical(hsi_sample_to_index(c(1, 1.5, 2), hsi_multi = 0.5),
                   tibble::tibble(Habitat = 1:4, Index = c(0, 1, 1, 1)))
  expect_identical(hsi_sample_to_index(c(1, 1.5, 2)),
                   tibble::tibble(Habitat = 0:2, Index = c(0, 1, 0.5)))
  expect_error(hsi_sample_to_index(1), "x must have at least 2 elements")
})
  