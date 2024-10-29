test_that("pref", {
  avail <- hsi_data
  avail$Index[avail$Index == 0] <- 1
  pref <- hsi_pref(hsi_data, avail)
  expect_identical(pref$Habitat, hsi_data$Habitat)
  expect_identical(pref$Index, c(0, rep(1, 8), 0))

  pref <- hsi_pref(hsi_data, hsi_data)
  expect_identical(pref$Habitat, hsi_data$Habitat[2:9])
  expect_identical(pref$Index, rep(1, 8))
})

test_that("pref and use", {
  avail <- hsi_data
  avail$Index[avail$Index == 0] <- 1
  pref <- hsi_pref(hsi_data, avail)
  use <- hsi_use(hsi_data, pref)
  expect_equal(use, hsi_data)
})
