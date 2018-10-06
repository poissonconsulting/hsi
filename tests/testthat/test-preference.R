context("pref")

test_that("pref", {
  pref <- hsi_pref(hsi_data, hsi_data)
  expect_identical(pref$Habitat, hsi_data$Habitat[1:9])
  expect_identical(pref$Index, rep(1, 9))
})

test_that("pref and use", {
  pref <- hsi_pref(hsi_data, hsi_data)
  use <- hsi_use(hsi_data, pref)
  expect_equal(use, hsi_data[1:9,])
})
