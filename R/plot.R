#' Plot HSI Data
#'
#' @param x A HSI data frame.
#' @inheritParams check_hsi
#'
#' @return A ggplot object
#' @export
#' @examples
#' hsi_plot(hsi_data)
hsi_plot <- function(x, habitat = "Habitat", index = "Index") {
  check_hsi(x, habitat, index, unique = FALSE)

  requireNamespace("ggplot2")

  ggplot2::ggplot(data = x, ggplot2::aes(x = x[[habitat]], y = x[[index]])) +
    ggplot2::geom_line() +
    ggplot2::expand_limits(y = c(0, 1))
}
