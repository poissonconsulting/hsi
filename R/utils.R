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

#' HSI Seq By
#'
#' @param x A numeric vector of habitat values. 
#' @param by A non-negative number of the habitat increments.
#' @return A numeric vector of equal spaced values spanning the range of x.
#' @export
#' @examples
#' hsi_seq_by(c(1,2,1.5))
#' hsi_seq_by(c(1,2,1.5), by = 1)
hsi_seq_by <- function(x, by = hsi_by(x)) {
  check_vector(x, 1, length = TRUE)
  check_scalar(by, c(0.001, 1000))

  min <- min(x)
  max <- max(x)
  min <- min - min %% by - by
  max <- max + max %% by + by
  
  seq <- seq(min, max, by = by)

  if(seq[length(seq)] > max(x) + by) seq <- seq[-length(seq)]
  if(seq[1] < min(x) - by) seq <- seq[-1]
  seq
}
