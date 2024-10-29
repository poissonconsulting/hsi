#' HSI Use
#'
#' @param use A hsi data frame of use
#' @param avail A hsi data frame of availability
#' @inheritParams check_hsi
#' @return A hsi data frame of preference for overlapping habitats.
#' @export
#'
#' @examples
#' hsi_pref(hsi_data, hsi_data)
hsi_pref <- function(use, avail, habitat = "Habitat", index = "Index") {
  check_hsi(use, habitat = habitat, index = index)
  check_hsi(avail, habitat = habitat, index = index, by = hsi_by(use[[habitat]]))

  use <- use[c(habitat, index)]
  avail <- avail[c(habitat, index)]

  avail <- avail[avail[[index]] != 0, ]
  pref <- merge(use, avail, by = habitat)
  if (!nrow(pref)) err("use and avail must have overlapping habitats")
  pref[[index]] <- pref[[paste0(index, ".x")]] / pref[[paste0(index, ".y")]]
  pref[[index]] <- pref[[index]] / max(pref[[index]])
  pref <- pref[c(habitat, index)]

  if (requireNamespace("tibble", quietly = TRUE)) pref <- tibble::as_tibble(pref)
  rownames(pref) <- NULL
  pref
}
