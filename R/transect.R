hsi_transect_to_index <- function(x, habitat = "Habitat", distance = "Distance", 
                                  by = hsi_by(x[[habitat]])) {
  check_vector(x, 1)
  check_scalar(by, c(0, 1000))
  
  x
}
