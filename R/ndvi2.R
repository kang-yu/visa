#' Calculate and plot a 2-band NDVI.
#'
#' This function calculates a 2-band NDVI using the \code{\link{nsr}} function.
#'
#' @details
#' Calculate a NDVI with two specific bands of choice. The new NDVI follows the
#' the standard formula \deqn{NDVI = (\lambda_i + \lambda_j)/(\lambda_i - \lambda_j)}.
#' Bands i and j correspond to the b1 and b2 input arguments, respectively. Wavelength
#' indexes are determined based on the first argument 's'.
#'
#' @rdname ndvi2
#' @inheritParams nsr
#' @return
#' \item{ndvi}{The returned values are the new NDVI.}
#' @examples
#' library(visa)
#' s <- NSpec.DF$spectra
#' ndvi2(s, 780, 680)
#'
#' @import ggplot2
#' @export
ndvi2 <- function(s, b1, b2){
  if (is.null(s)) stop("input s is not valid spectra")
  ndvi <- nsr(s, b1, b2)
}

