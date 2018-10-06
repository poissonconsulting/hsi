#' Check HSI Data
#'
#' A HSI data frame is a data frame with an integer Habitat column 
#' and a double Index column and an optional attribute hsi_multiplier 
#' indicating the habitat multiplier.
#' 
#' @inheritParams checkr::check_data
#' @param habitat A string of the name of the column with habitat values.
#' @param index A string of the name of the column with index values.
#' @param unique A flag indicating whether the habitat values must be unique.
#' @param sorted A flag indicating whether the habitat values must be sorted.
#'
#' @return An invisible copy of the original object.
#' @export
#'
#' @examples
#' check_hsi(hsi_data)
check_hsi <- function(x, habitat = "Habitat", index = "Index", 
                      hsi_multi = hsi_multiplier(x),
                      unique = TRUE,
                      sorted = unique,
                      x_name = substitute(x)) {
  x_name <- deparse(x_name)
  
  check_string(habitat)
  check_string(index)
  check_string(x_name)
  check_flag(unique)
  check_flag(sorted)
  
  check_data(x, c(habitat, index), nrow = TRUE, x_name = x_name)
  
  check_vector(x[[habitat]], 1L, unique = unique, sorted = sorted, x_name =
                 paste0("column '", habitat, "' of ", x_name))
  
  check_vector(x[[index]], c(0, 1), x_name =
                 paste0("column '", index, "' of ", x_name))
  
  if(unique && sorted && x[[habitat]][nrow(x)] - x[[habitat]][1] != nrow(x) - 1L)
    err("column '", habitat, "' of ", x_name, " must be consecutive values")

  check_scalar(hsi_multi, c(1e-06, 1e+06))

  if(hsi_multiplier(x) != hsi_multi) 
    err(paste0("hsi multiplier of '", x_name, 
               "' must be ", hsi_multi, " not ", hsi_multiplier(x)))
  invisible(x)
}

#' Check Transect
#' 
#' A transect data frame is a data frame with an unique sorted double Distance column and a double Habitat column.
#'
#' @inheritParams checkr::check_data
#' @param distance A string of the name of the column with distance values.
#' @param habitat A string of the name of the column with habitat values
#'
#' @return An invisible copy of the original object.
#' @export
#'
#' @examples
#' check_transect(trans_data)
check_transect <- function(x, distance = "Distance", habitat = "Habitat",
                           x_name = substitute(x)) {
  x_name <- deparse(x_name)
  check_string(distance)
  check_string(habitat)
  check_data(x, c(distance, habitat), nrow = TRUE)
  
  check_vector(x[[distance]], 1, unique = TRUE, sorted = TRUE, x_name =
                 paste0("column '", distance, "' of ", x_name))
  
  check_vector(x[[habitat]], 1, x_name =
                 paste0("column '", habitat, "' of ", x_name))
  invisible(x)
}
