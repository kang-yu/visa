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
#' @examples
#' \dontrun{
#' library(visa)
#' x <- NSpec.DF$N
#' s <- NSpec.DF$spectra
#' sr1 <- sr(s, 440, 445)
#' }
#' @import ggplot2 ggpmisc
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
#' @examples
#' \dontrun{
#' s <- NSpec.DF
#' y <- NSpec.DF$N
#' lm.sr(s,400,500,y)
#' }
#' @export
lm.sr <- function(s,b1,b2,y){

  x <- sr(s,b1,b2)
  bstr <- paste("SR = R", b1, "/R", b2, sep = "")

  p <- plot.fit(x,y)+
    labs(x = bstr)
  p
}


