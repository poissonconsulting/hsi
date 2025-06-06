#' Sample to Index
#'
#' @param x A double vector of habitat values.
#' @param by The increments for the hsi Habitat values.
#' @return A hsi data frame with columns Habitat and Index.
#' @export
#'
#' @examples
#' hsi_sample_to_index(runif(100, 1, 2), by = 0.1)
hsi_sample_to_index <- function(x, by = hsi_by(x)) {
  chk_vector(x)
  check_values(x, 1)
  check_dim(x, values = TRUE)
  chk_scalar(by)
  chk_range(by, c(0.0001, 1000))

  data <- data.frame(Habitat = hsi_seq_by(x, by = by))

  index <- cut(x, breaks = data$Habitat, right = FALSE)
  index <- table(index)
  index <- as.vector(index)
  index <- index / max(index)

  data <- data[-nrow(data), , drop = FALSE]
  data$Index <- index

  rlang::check_installed("tibble")
  data <- tibble::as_tibble(data)
  rownames(data) <- NULL

  data
}
