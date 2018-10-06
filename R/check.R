#' Check HSI Data
#'
#' A HSI data frame is a data frame with a Habitat column 
#' and Index column.
#' 
#' @param habitat A string of the name of the column with habitat values.
#' @param index A string of the name of the column with index values.
#' @param unique A flag indicating whether the habitat values must be unique.
#' @param sorted A flag indicating whether the habitat values must be sorted.
#' @inheritParams checkr::check_data
#' @return An invisible copy of the original object.
#' @export
#'
#' @examples
#' check_hsi(hsi_data)
check_hsi <- function(x, habitat = "Habitat", index = "Index", 
                      unique = TRUE,
                      sorted = unique,
                      x_name = substitute(x)) {
  x_name <- deparse(x_name)
  
  check_string(habitat)
  check_string(index)
  check_flag(unique)
  check_flag(sorted)
  check_string(x_name)
  
  check_data(x, c(habitat, index), x_name = x_name)
  
  check_vector(x[[habitat]], 1, unique = unique, sorted = sorted, x_name =
                 paste0("column '", habitat, "' of ", x_name))
  
  check_vector(x[[index]], c(0, 1), x_name =
                 paste0("column '", index, "' of ", x_name))
  invisible(x)
}

#' Check Transect
#' 
#' A transect data frame is a data frame with Distance column and a Habitat column.
#'
#' @inheritParams check_hsi
#' @param distance A string of the name of the column with distance values.
#' @param unique A flag indicating whether the distance values must be unique.
#' @param sorted A flag indicating whether the distance values must be sorted.
#' @return An invisible copy of the original object.
#' @export
#'
#' @examples
#' check_transect(trans_data)
check_transect <- function(x, distance = "Distance", habitat = "Habitat",
                           unique = TRUE,
                           sorted = unique,
                           x_name = substitute(x)) {
  x_name <- deparse(x_name)
  
  check_string(distance)
  check_string(habitat)
  check_flag(unique)
  check_flag(sorted)
  check_string(x_name)
  
  check_data(x, c(distance, habitat))
  
  check_vector(x[[distance]], 1, unique = unique, sorted = sorted, x_name =
                 paste0("column '", distance, "' of ", x_name))
  
  check_vector(x[[habitat]], 1, x_name =
                 paste0("column '", habitat, "' of ", x_name))
  invisible(x)
}
