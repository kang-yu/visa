#' Calculate simple ratio (SR) indices.
#'
#' descriptions
#'
#' details
#'
#' @param x,y numeric vectors.
sr <- function(x, y) x/y


#' Calculate normalized simple ratio (NSR) indices.
#'
#' descriptions
#'
#' @rdname sr
nsr <- function(x, y){
  (x - y)/(x + y)
}

