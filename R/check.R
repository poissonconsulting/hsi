#' Check HSI Data
#'
#' @inheritParams checkr::check_data
#' @param habitat A string of the name of the column with habitat values.
#' @param index A string of the name of the column with index values.
#' @param by A non-negative number of the habitat increments.
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

  if(identical(habitat, index))
    err("arguments 'habitat' and 'index' must specify different columns")

  check_colnames(x, c(habitat, index))

  check_data(x, nrow = TRUE, x_name = x_name)

  check_vector(x[[habitat]], 1, unique = TRUE, sorted = TRUE, x_name =
                 paste0("column '", habitat, "' of ", x_name))

  check_vector(x[[index]], c(0, 1), x_name =
                 paste0("column '", index, "' of ", x_name))
  
  by <- force(by)
  check_scalar(by, c(0, 1000))
  
  diff <- diff(x[[habitat]])
  if(any(diff != diff[1]))
    err("column '", habitat, "' of ", x_name, "must have equal increments")
  
  if(hsi_by(x[[habitat]]) != by) 
    err(paste0("column '", habitat, "' of ", x_name, 
               " increments must be ", by, " not ", hsi_by(x[[habitat]])))
  invisible(x)
}
