#' HSI By
#'
#' @param x A numeric vector of habitat values. 
#' @return The smallest habitat increment.
#' @export
#' @examples
#' hsi_by(c(1, 1.5, 2))
hsi_by <- function(x) {
  check_vector(x, 1, length = TRUE)
  x <- unique(x)
  x <- sort(x)
  x <- diff(x)
  if(!length(x)) return(0)
  min(x)
}
