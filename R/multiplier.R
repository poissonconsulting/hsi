#' HSI Multiplier
#'
#' @param x A HSI object.
#'
#' @return A double scalar of the hsi multiplier.
#' @export
hsi_multiplier <- function(x) {
  hsi_multiplier <- attr(x, "hsi_multi")
  if(is.null(hsi_multiplier)) return(1)
  hsi_multiplier
}

#' HSI Multiplier
#'
#' @param x A HSI object.
#'
#' @return A double scalar of the hsi multiplier.
#' @export
hsi_set_multiplier <- function(x, hsi_multi = hsi_multiplier(x)) {
  check_hsi(x)
  
  hsi_multiplier <- attr(x, "hsi_multi")
  if(is.null(hsi_multiplier)) return(1)
  hsi_multiplier
}
