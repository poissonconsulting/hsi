#' Sample to Index
#'
#' @param x A double vector of habitat values.
#' @param hsi_multi The hsi multiplier for the resultant object
#' @return A hsi data frame with columns Habitat and Index.
#' @export
#'
#' @examples
#' hsi_sample_to_index(runif(100, 1, 2))
#' hsi_sample_to_index(runif(100, 1, 2), hsi_multi = 0.1)
hsi_sample_to_index <- function(x, hsi_multi = 1) {
  check_vector(x, 1, length = TRUE)
  check_hsi_multiplier(hsi_multi)

  x <- x / hsi_multi
  min <- floor(min(x))
  max <- ceiling(max(x))
  
  data <- data.frame(Habitat = (min - 1L):(max + 1L))
  if(requireNamespace("tibble", quietly = TRUE)) data <- tibble::as_tibble(data)
  rownames(data) <- NULL
  attr(data, "hsi_multi") <- hsi_multi
  
  index <- cut(x, breaks = data$Habitat, right = FALSE)
  index <- table(index)
  index <- as.vector(index)
  data$Index <- c(index,0) / max(index)
  data
}
