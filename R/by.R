#' HSI By
#'
#' @param x A numeric vector of habitat values. 
#' @return A number of the habitat increments.
#' @export
#' @examples
#' hsi_by(c(1, 1.5, 2))
hsi_by <- function(x) {
  check_vector(x, 1, length = TRUE, unique = TRUE, sorted = TRUE)
  x <- diff(x)
  if(!length(x)) return(0)
  if(any(x != x[1])) err("hsi habitat must be evenly spaced")
  x[1]
}

#' HSI By
#'
#' @param x A HSI data frame.
#' @inheritParams check_hsi
#' @return The modified HSI data frame.
#' @export
#' @examples
#' hsi_by(c(1, 1.5, 2))
hsi_set_by <- function(x, habitat = "Habitat", index = "Index", 
                       by = hsi_by(x[[habitat]])) {
  check_hsi(x)
  check_scalar(by, c(0, 1000))
  if(hsi_by(x) == by) return(x)
  err()
  x
}
