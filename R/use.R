#' HSI Use
#'
#' @param pref A hsi data frame of preference
#' @param avail A hsi data frame of availability
#' @inheritParams check_hsi
#' @return A hsi data frame of use for overlapping habitats.
#' @export
#'
#' @examples
#' hsi_use(hsi_data, hsi_data)
hsi_use <- function(pref, avail, habitat = "Habitat", index = "Index") {
  check_hsi(pref, habitat = habitat, index = index)
  check_hsi(avail, habitat = habitat, index = index)

  pref <- pref[c(habitat, index)]
  avail <- avail[c(habitat, index)]
  
  use <- merge(pref, avail, by = habitat)
  if(!nrow(use)) err("pref and avail must have overlapping habitat")
  use[[index]] <- use[[paste0(index, ".x")]] * use[[paste0(index, ".y")]]
  use[[index]] <- use[[index]] / max(use[[index]])
  use <- use[c(habitat, index)]

  if(requireNamespace("tibble", quietly = TRUE)) use <- tibble::as_tibble(use)
  rownames(use) <- NULL
  use
}
