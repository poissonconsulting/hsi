#' Plot HSI Data
#'
#' @param x A data frame with
#' @inheritParams check_hsi
#'
#' @return A ggplot object
#' @export
#' @examples 
#' hsi_plot(hsi_data)
hsi_plot <- function(x, habitat = "Habitat", index = "Index") {
  requireNamespace("ggplot2")
  check_hsi(x, habitat, index)
  
  ggplot2::ggplot(data = x, ggplot2::aes_string(x = habitat, y = index)) +
    ggplot2::geom_path() +
    ggplot2::expand_limits(y = 0)
}
