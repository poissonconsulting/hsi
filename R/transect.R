#' Transect to Sample
#'
#' @param x A data frame.
#' @param distance A string of the column name with transect distance values.
#' @param habitat A string of the column name with habitat values.
#' @param n A count of the number of samples to take.
#'
#' @return A vector of samples.
#' @export
#'
#' @examples
#' hsi_transect_to_sample(trans_data, n = 10)
hsi_transect_to_sample <- function(
    x, distance = "Distance", habitat = "Habitat", n = 10^6) {
  check_transect(x, distance, habitat)
  stats::approx(x = x[[distance]], y = x[[habitat]], n = n)$y
}

#' Transect to HSI Index
#'
#' @inheritParams hsi_transect_to_sample
#' @inheritParams check_hsi
#' @inheritParams hsi_seq_by
#' @return A hsi data frame
#' @export
#' @examples
#' hsi_transect_to_index(trans_data)
hsi_transect_to_index <- function(
    x, distance = "Distance", habitat = "Habitat", by = 1, n = 10^6) {
  sample <- hsi_transect_to_sample(x, distance, habitat, n = n)
  hsi_sample_to_index(sample, by = by)
}

#' Transect Set By
#'
#' @inheritParams check_transect
#' @inheritParams hsi_seq_by
#' @return A HSI data.
#' @export
#' @examples
#' hsi_transect_set_by(trans_data)
hsi_transect_set_by <- function(x, distance = "Distance", habitat = "Habitat", by = hsi_by(x[[distance]])) {
  check_transect(x, distance, habitat)
  chk_scalar(by)
  chk_range(by, c(0.001, 1000))

  seq <- hsi_seq_by(x[[distance]], by = by)
  data <- data.frame(Distance = seq)
  colnames(data) <- distance
  data[[habitat]] <- stats::approx(x[[distance]], x[[habitat]], xout = seq)$y
  data <- data[!is.na(data[[habitat]]), ]
  rlang::check_installed("tibble")
  data <- tibble::as_tibble(data)
  rownames(data) <- NULL
  data
}
