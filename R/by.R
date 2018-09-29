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

#' HSI Set By
#'
#' @inheritParams check_hsi
#' @return A HSI data.
#' @export
#' @examples
#' hsi_set_by(hsi_data, by = 2)
hsi_set_by <- function(x, habitat = "Habitat", index = "Index", by = hsi_by(x[[habitat]])) {
  check_hsi(x, habitat, index, by = NULL)
  check_scalar(by, c(0.001, 1000))
  
  seq <- hsi_seq_by(x[[habitat]], by = by)
  data <- data.frame(Habitat = seq)
  if(requireNamespace("tibble", quietly = TRUE)) data <- tibble::as_tibble(data)
  rownames(data) <- NULL
  data$Index = stats::approx(x[[habitat]], x[[index]], xout = seq)$y
  data <- data[!is.na(data$Index),]
  data$Index <- data$Index / max(data$Index)
  
  data
}

