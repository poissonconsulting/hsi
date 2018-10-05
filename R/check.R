#' Check HSI Data
#'
#' A HSI data frame is a data frame with a column of habitat and index values.
#' 
#' @inheritParams checkr::check_data
#' @param habitat A string of the name of the column with habitat values.
#' @param index A string of the name of the column with index values.
#' @param by A non-negative number of the habitat increments.
#' If by = NULL then does not check habitat increments
#'
#' @return An invisible copy of the original object.
#' @export
#'
#' @examples
#' check_hsi(hsi_data)
check_hsi <- function(x, habitat = "Habitat", index = "Index", 
                      by = hsi_by(x[[habitat]]),
                      x_name = substitute(x)) {
  x_name <- deparse(x_name)
  
  check_string(habitat)
  check_string(index)
  check_string(x_name)
  
  check_data(x, c(habitat, index), nrow = TRUE, x_name = x_name)
  
  check_vector(x[[habitat]], 1, unique = TRUE, sorted = TRUE, x_name =
                 paste0("column '", habitat, "' of ", x_name))
  
  check_vector(x[[index]], c(0, 1), x_name =
                 paste0("column '", index, "' of ", x_name))
  
  by <- force(by)
  checkor(check_scalar(by, c(0.001, 1000)), check_null(by))
  
  if(!is.null(by)) {
    diff <- diff(x[[habitat]])
    if(!all(vapply(diff, all.equal, TRUE, diff[1],
                   check.attributes = FALSE)))
      err("column '", habitat, "' of ", x_name, " must have equal increments")
    
    if(hsi_by(x[[habitat]]) != by) 
      err(paste0("column '", habitat, "' of ", x_name, 
                 " increments must be ", by, " not ", hsi_by(x[[habitat]])))
    
    seq_by <- hsi_seq_by(x[[habitat]])
    if(!x[[habitat]][1] %in% seq_by[1:2]) {
      err(paste0("column '", habitat, "' of ", x_name, 
                 " must have ", seq_by[2], " in its sequence"))
    }
  }
  invisible(x)
}

#' Check Transect
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
