seq_by <- function(x, by) {
  min <- min(x)
  max <- max(x)
  min <- min - min %% by - by
  max <- max + max %% by + by
  
  seq(min, max, by = by)   
}