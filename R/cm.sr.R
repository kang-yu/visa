#' Selecting the best 2-Band combinations for Simple Ratio (SR)
#'
#' @name cm.sr
#' @description This function develops a optimization algorithm based on correlation analysis between spectral matrix 'spectra' and the
#' vegetation variable of interest x, which
#' determines the best spectral band combinations of the full spectrum that are most predictive for 'x'.
#'
#' @details This function runs a calculation of \deqn{ NDVI = \lambda_i / \lambda_j } using all the possible pairs/combinations of any two bands (i,j)
#' within the full spectrum range thoroughly. A correlation analysis is then performed between the x and all possible NDVIs, and it calculates
#' the correlation coefficients (r) which indicates the predictive performance of each NDVI and its corresponding two-band combination. The
#' output is the wavelength (nm) indicating the best two bands that produce the highest value of r.
#'
#' @inheritParams cm.nsr
#' @return
#'   \item{cm}{Returns a correlation coefficients matrix.}
#' @seealso \code{\link{cm.nsr}}
#' @examples
#' library(visa)
#' data(NSpec.DF)
#' x <- NSpec.DF$N # nitrogen
#' S <- NSpec.DF$spectra[, seq(1, ncol(NSpec.DF$spectra), 5)] # resampled to 10 nm steps
#' cm <- cm.sr(S, x, cm.plot = FALSE)
#'
#' @import ggplot2 Matrix reshape2 grDevices
#' @export

cm.sr <- function(S, x, w = wavelength(S), w.unit = NULL, cm.plot = FALSE){

  # account for the format of spectra
  # check nsr

  spectra <- spectra(S)
  if (is.matrix(spectra) && is.null(colnames(spectra)) && length(w) == 0)
    stop("Wavelength for the spectra matrix is not correctly defined")

  n <- dim(spectra)[2] # Returns the Number of wavebands, should equal w

  dn <- list(paste0("i_", w), paste0("j_",w))
  ## Ri/Rj

  R2 <- Matrix::Matrix(0, n, n, dimnames = dn, sparse = TRUE)  # Zero sparse matrix
  # str(R2)
  Rj <- spectra

  ones <- matrix(1,1,n)

  for (cI in 1:n){
    Ri <- spectra[,cI]
    Ri <- Ri %*% ones  # to matrix
    # VI formular
    V <- Ri/Rj
    # Squared values (R2) of the Pearson Correlation coefficients
    Rcorr <- (stats::cor(x, V))^2
    # To store the value of R2
    spR <- Matrix::sparseMatrix(i = rep(cI,n),j = c(1:n), x = as.numeric(Rcorr), dims = c(n,n), dimnames = dn)
    R2 <- R2 + spR
    # image(R2)
  }
  cm <- Matrix::as.matrix(R2)
  R2max <- max(cm, na.rm = TRUE)
  print(paste('The max value of R^2 is', as.character(round(R2max,4))))
  ind_max <- which(cm == R2max, arr.ind = TRUE)

  # ind_max
  bestBands = w[ind_max[1,]]
  print(paste(c("i", "j"), as.vector(bestBands), sep = "_"))

  # str(cm)
  # max(cm, na.rm = TRUE)
  colnames(cm) <- paste(w, "nm")

  # cm plot
  cm_plot <- ggplot.cm(cm, show.stat = FALSE)
  if (isTRUE(cm.plot)) print(cm_plot)
  # cm.res <- list(cm = cm, cm.plot = cm_plot)

  cm <- cm
}
