context("sample")

test_that("sample_to_index", {
  expect_equal(
    check_hsi(hsi_sample_to_index(c(1, 1.5, 2), hsi_multi = 0.5), hsi_multi = 0.5),
    tibble::tibble(Habitat = 1:5, Index = c(0, 1, 1, 1, 0)))
  expect_equal(hsi_sample_to_index(c(1, 1.5, 2)),
                   tibble::tibble(Habitat = 0:3, Index = c(0, 1, 0.5, 0)))
  expect_equal(hsi_sample_to_index(1), 
                   tibble::tibble(Habitat = 0:2, Index = c(0, 1, 0)))
})
