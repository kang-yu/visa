
#' Calculate Normalized Simple Ratio (NSR) index.
#'
#' It is a normalization of SR by doing NSR = (1-SR)/(1+SR), with the same two spectral bands.
#'
#' As it exactly reads in its name, it is a normalization of the SR and ranges in (0,1).
#'
#' @rdname nsr
#' @inheritParams sr
#'
#' @return
#'   \item{nsr}{Returns a NSR index.}
#' @examples
#' s <- NSpec.DF$spectra
#' nsr1 <- nsr(s, 480, 550)
#'
#' @export
nsr <- function(s, b1, b2){
  sr_val <- sr(s, b1, b2)
  nsr <- (1 - sr_val)/(1+ sr_val)
  return(nsr)
}


#' Fit linear model for the Normalized Simple Ratio (NSR) and another variable.
#'
#' @rdname nsr
#' @inheritParams lm.sr
#'
#' @return
#'   \item{p}{Returns a ggplot object.}
#' @examples
#' s <- NSpec.DF
#' y <- NSpec.DF$N
#' lm.nsr(s,600,500,y)
#'
#' @export
lm.nsr <- function(s,b1,b2,y){

  x <- nsr(s,b1,b2)
  bstr <- paste("SR = R", b1, "/R", b2, sep = "")

  p <- ggplot.lmfit(x,y)+
    labs(x = bstr)
  p
}

