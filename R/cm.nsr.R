#' Selecting the best 2-Band combinations for Normalized Simple Ratio (NSR)
#'
#' This function develops a optimization algorithm based on correlation analysis between spectral matrix 'spectra' and the
#' vegetation variable of interest x, which
#' determines the best spectral band combinations of the full spectrum that are most predictive for 'x'.
#'
#' @param S A matrix of spectral data, a row is a spectrum across all spectral bands.
#' @param x A vector.
#' @param w A vector of wavelength.
#' @param w.unit Character string, default = NULL,
#' @param cm.plot A logic value for whether plotting the coefficient matrix or not, default FALSE.
#' @return
#'   \item{cm}{Returns a correlation coefficients matrix.}
#'
#' @details
#' This function runs a calculation of \deqn{ NDVI = (\lambda_i - \lambda_j)/(\lambda_i + \lambda_j) } using all the possible pairs/combinations of any two bands (i,j)
#' within the full spectrum range thoroughly. A correlation analysis is then performed between the x and all possible NDVIs, and it calculates
#' the correlation coefficients (r) which indicates the predictive performance of each NDVI and its corresponding two-band combination. The
#' output is the wavelength (nm) indicating the best two bands that produce the highest value of r.
#' @seealso \code{\link{cor}}
#' @examples
#' \dontrun{
#' data(NSpec.DF)
#' x <- NSpec.DF$N # nitrogen
#' S <- NSpec.DF$spectra[, seq(1, ncol(NSpec.DF$spectra), 5)] # resampled to 10 nm steps
#' cm.nsr(S, x, cm.plot = TRUE)
#' }
#' @import ggplot2 Matrix reshape2 grDevices
#' @export

cm.nsr <- function(S, x, w = wavelength(S), w.unit = NULL, cm.plot = FALSE){

  # determin the format of spectra
  # if (is(spectra, "Spectra")) w <- wavelength(spectra)
  # if (is(spectra, "data.frame")) w <- wavelength(spectra) # shoudl be numeric
  # if (is(spectra, "matrix")) w <- wavelength(spectra) # shoudl be numeric
  # n <- length(spectra)

  spectra <- spectra(S)
  if (is.matrix(spectra) && is.null(colnames(spectra)) && length(w) == 0)
    stop("Wavelength for the spectra matrix is not correctly defined")

  n <- dim(spectra)[2] # Returns the Number of wavebands, should equal w

  ## (Rj-Ri)/(Rj+Ri)

  R2 <- Matrix::Matrix(0, n, n, sparse = TRUE)  # Zero sparse matrix
  Rj <- spectra

  ones <- matrix(1,1,n)

  for (cI in 1:n){
    Ri <- spectra[,cI]
    Ri <- Ri %*% ones  # to matrix
    # VI formular
    V <- (Rj-Ri)/(Rj+Ri)
    # Squared values (R2) of the Pearson Correlation coefficients
    Rcorr <- (stats::cor(x, V))^2
    # To store the value of R2
    # spR <- Matrix::sparseMatrix(i = c(1:n),j = rep(cI,n), x = as.numeric(Rcorr), dims = c(n,n))
    spR <- Matrix::sparseMatrix(i = rep(cI,n), j =  c(1:n), x = as.numeric(Rcorr), dims = c(n,n))
    R2 <- R2 + spR
  }
  cm <- as.matrix(R2)
  # str(cm)
  # max(cm, na.rm = TRUE)
  colnames(cm) <- paste(w, "nm")

  R2max <- max(cm, na.rm = TRUE)
  print(paste('The max value of R^2 is', as.character(round(R2max,4))))
  ind_max <- which(cm == R2max, arr.ind = TRUE)
  bestBands = w[ind_max[1,]]
  print(paste(c("i", "j"), as.vector(bestBands), sep = "_"))

  # cm plot
  cm_plot <- ggplot.cm(cm, show.stat = FALSE)
  if (isTRUE(cm.plot)) print(cm_plot)
  # cm.res <- list(cm = cm, cm.plot = cm_plot)

  cm <- cm
}
