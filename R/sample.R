#' Sample to Index
#'
#' @param x A double vector.
#' @param by A non-negative number of the habitat increments.
#' @return A hsi data frame with columns Habitat and Index.
#' @export
#'
#' @examples
#' hsi_sample_to_index(runif(100, 1, 2), by = 0.1)
hsi_sample_to_index <- function(x, by = hsi_by(x)) {
  check_vector(x, 1, length = TRUE)
  check_scalar(by, c(0.0001, 1000))
  
  data <- data.frame(Habitat = seq_by(x, by = by))
  if(requireNamespace("tibble", quietly = TRUE)) data <- tibble::as_tibble(data)
  rownames(data) <- NULL
  
  index <- cut(x, breaks = c(data$Habitat), right = FALSE)
  index <- table(index)
  index <- as.vector(index)
  data <- data[-nrow(data),]
  data$Index <- index / max(index)
  data <- data[-1,]
  data
}
