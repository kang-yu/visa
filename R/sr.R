#' Calculate SR and NSR indices
#'
#' @param x,y numeric vectors.
#' @return
sr <- function(x, y) x / y

#' @rdname sr
nsr <- function(x, y){
  (x - y) /(x + y)
}
