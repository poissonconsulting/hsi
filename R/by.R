#' HSI By
#'
#' @param x A vector of habitat values.
#' @return A number of the increments
#' @export
#'
#' @examples
#' hsi_by(c(1, 2, 1.5))
hsi_by <- function(x) {
  chk_vector(x)
  check_values(x, 1)
  check_dim(x, values = TRUE)
  x <- unique(x)
  x <- sort(x)
  x <- diff(x)
  if (!length(x)) {
    return(0)
  }
  min(x)
}

#' HSI Sequence
#'
#' @param x A vector of habitat values
#' @param by A number of the increments.
#'
#' @return A vector of the sequence
#' @export
#'
#' @examples
#' hsi_seq_by(c(1, 2, 1.5))
hsi_seq_by <- function(x, by = hsi_by(x)) {
  chk_vector(x)
  check_values(x, 1)
  check_dim(x, values = TRUE)
  chk_scalar(by)
  chk_range(by, c(0.001, 1000))

  min <- min(x)
  max <- max(x)
  seq_min <- min - min %% by - by
  seq_max <- max - max %% by + 2 * by

  seq <- seq(seq_min, seq_max, by = by)

  seq
}

#' HSI Set By
#'
#' @param x A HSI data frame.
#' @inheritParams check_hsi
#' @inheritParams hsi_seq_by
#' @return A HSI data.
#' @export
#' @examples
#' hsi_set_by(hsi_data, by = 2)
hsi_set_by <- function(x, habitat = "Habitat", index = "Index", by = hsi_by(x[[habitat]])) {
  check_hsi(x, habitat, index, unique = TRUE)
  chk_scalar(by)
  chk_range(by, c(0.001, 1000))

  seq <- hsi_seq_by(x[[habitat]], by = by)
  seq <- seq[-c(1, length(seq))]
  data <- data.frame(Habitat = seq)
  data$Index <- stats::approx(x[[habitat]], x[[index]], xout = seq)$y
  data$Index[is.na(data$Index)] <- 0
  n <- nrow(data)
  if (n >= 2 && identical(data$Index[c(n - 1, n)], c(0, 0))) {
    data <- data[-n, ]
  }
  data$Index <- data$Index / max(data$Index)
  if (requireNamespace("tibble", quietly = TRUE)) data <- tibble::as_tibble(data)
  rownames(data) <- NULL

  data
}
