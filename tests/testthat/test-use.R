test_that("use", {
  use <- hsi_use(hsi_data, hsi_data)
  expect_identical(use$Habitat, hsi_data$Habitat)
  expect_identical(use$Index, hsi_data$Index^2)
})
