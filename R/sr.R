#' Calculate Simple Ratio (SR).
#'
#' Simple Ratio is the ratio of the spectra (mostly reflectance) between two bands
#' in the format of \deqn{SR = \lambda_i/\lambda_j}
#'
#' Simple ratio and NDVI looking indices are the two groups of mostly used spectral indices in remote sensing.
#'
#' @param s Spectral data in the format of visa's Spectra object, spectra.data.frame or spectra.matrix.
#' @param b1 A integer number which defines the wavelength of the 1st spectral band.
#' @param b2 A integer number which defines the wavelength of the 2nd spectral band.
#'
#' @return
#'   \item{sr}{Returns a simple ratio index.}
#'
#' @examples
#' library(visa)
#' s <- NSpec.DF$spectra
#' sr1 <- sr(s, 480, 550)
#'
#' @import ggplot2
#'
#' @export

sr <- function(s, b1, b2){
  spec <- spectra(s)
  wl <- wavelength(s)
  idx1 <- b1 == wl
  idx2 <- b2 == wl
  s1 <- spec[, idx1]
  s2 <- spec[, idx2]
  sr <- s1/s2
}

#' Calculate Normalized Simple Ratio (NSR) index.
#'
#' It is a normalization of SR by doing NSR = (1-SR)/(1+SR), with the same two spectral bands.
#'
#' As it exactly reads in its name, it is a normalization of the SR and ranges in (0,1).
#'
#' @rdname sr
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
  sr <- sr(s, b1, b2)
  nsr <- (1 - sr)/(1+ sr)
}


#' Fit linear model for the Simple Ratio (SR) and another variable.
#'
#' @rdname sr
#' @inheritParams sr
#' @param y A numeric variable to correlate with SR
#'
#' @return
#'   \item{p}{Returns a ggplot object.}
#' @examples
#' s <- NSpec.DF
#' y <- NSpec.DF$N
#' lm.sr(s,600,500,y)
#'
#' @export
lm.sr <- function(s,b1,b2,y){

  x <- sr(s,b1,b2)
  bstr <- paste("SR = R", b1, "/R", b2, sep = "")

  p <- ggplot.lmfit(x,y)+
    labs(x = bstr)
  p
}
#' Fit linear model for the Normalized Simple Ratio (NSR) and another variable.
#'
#' @rdname sr
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

  x <- sr(s,b1,b2)
  bstr <- paste("SR = R", b1, "/R", b2, sep = "")

  p <- ggplot.lmfit(x,y)+
    labs(x = bstr)
  p
}

