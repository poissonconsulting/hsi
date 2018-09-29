context("plot")

test_that("plot", {
  expect_is(hsi_plot(hsi_data), "ggplot")
})