#' Check HSI Data
#'
#' A HSI data frame is a data frame with a Habitat column 
#' and Index column.
#' 
#' @param habitat A string of the name of the column with habitat values.
#' @param index A string of the name of the column with index values.
#' @inheritParams hsi_seq_by
#' @param unique A flag indicating whether the habitat values must be unique.
#' @param sorted A flag indicating whether the habitat values must be sorted.
#' @inheritParams chk::check_data
#' @return An invisible copy of the original object.
#' @export
#'
#' @examples
#' check_hsi(hsi_data)
check_hsi <- function(x, habitat = "Habitat", index = "Index", 
                      by = hsi_by(x[[habitat]]),
                      unique = TRUE,
                      sorted = unique,
                      x_name = substitute(x)) {
  x_name <- deparse(x_name)
  
  chk_string(habitat)
  chk_string(index)
  chk_flag(unique)
  chk_flag(sorted)
  chk_string(x_name)
  
  check_names(x, c(habitat, index), x_name = x_name)
  
  chk_scalar(by)
  chk_range(by, c(0.001, 1000))
  
  chk_vector(x[[habitat]], x_name = paste0("column '", habitat, "' of ", x_name))
  check_values(x[[habitat]], 1, x_name = paste0("column '", habitat, "' of ", x_name))
  chk_unique(x[[habitat]], x_name = paste0("column '", habitat, "' of ", x_name))
  chk_sorted(x[[habitat]], x_name = paste0("column '", habitat, "' of ", x_name))

  chk_vector(x[[index]], x_name = paste0("column '", index, "' of ", x_name))
  check_values(x[[index]], c(0, 1), x_name = paste0("column '", index, "' of ", x_name))

  
  if(!is.null(by)) {
    if(hsi_by(x[[habitat]]) != by) 
      err(paste0("column '", habitat, "' of ", x_name, 
                 " increments must be ", by, " not ", hsi_by(x[[habitat]])))
    
    diff <- diff(sort(unique(x[[habitat]])))
    if(!all(vapply(diff, all.equal, TRUE, by)))
      err("column '", habitat, "' of ", x_name, " must have equal increments")
  }
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
  
  chk_string(distance)
  chk_string(habitat)
  chk_flag(unique)
  chk_flag(sorted)
  chk_string(x_name)
  
  check_names(x, c(distance, habitat))
  
  chk_vector(x[[distance]], x_name = paste0("column '", distance, "' of ", x_name))
  check_values(x[[distance]], 1, x_name = paste0("column '", distance, "' of ", x_name))
  chk_unique(x[[distance]], x_name = paste0("column '", distance, "' of ", x_name))
  chk_sorted(x[[distance]], x_name = paste0("column '", distance, "' of ", x_name))
  
  chk_vector(x[[habitat]], x_name = paste0("column '", habitat, "' of ", x_name))
  check_values(x[[habitat]], 1, x_name = paste0("column '", habitat, "' of ", x_name))
  invisible(x)
}
