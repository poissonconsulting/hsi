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
  check_vector(x, 1, length = TRUE)
  check_scalar(by, c(0.0001, 1000))
  
  data <- data.frame(Habitat = hsi_seq_by(x, by = by))

  index <- cut(x, breaks = data$Habitat, right = FALSE)
  index <- table(index)
  index <- as.vector(index)
  index <- index / max(index)

  data <- data[-nrow(data),,drop = FALSE]
  data$Index <- index

  if(requireNamespace("tibble", quietly = TRUE)) data <- tibble::as_tibble(data)
  rownames(data) <- NULL

  data
}
