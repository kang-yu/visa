#' Selecting the best 2-Band combinations for Normalized Simple Ratio (NSR)
#'
#' This function develops an optimization algorithm based on correlation analysis between the spectral matrix \code{spectra} and the
#' vegetation variable of interest \code{x}. It determines the best spectral band combinations (i, j) of the full spectrum that are most predictive for \code{x}.
#'
#' @param S A matrix of spectral data, where each row is a spectrum across all spectral bands.
#' @param x A numeric vector (e.g., a vegetation variable).
#' @param w A numeric vector of wavelengths; by default it is derived using \code{wavelength(S)}.
#' @param w.unit A character string specifying the unit of wavelengths (default is \code{NULL}).
#' @param cm.plot Logical. If \code{TRUE}, the correlation coefficient matrix is plotted.
#'
#' @return
#'   \item{cm}{A correlation coefficient matrix with squared Pearson correlation values.}
#'
#' @details
#' For every possible pair of distinct bands (i, j), the function calculates
#' \deqn{ \mathrm{NSR} = \frac{R_j - R_i}{R_j + R_i} }
#' and then computes the squared Pearson correlation (\eqn{R^2}) between \code{x} and the resulting NSR values.
#'
#' If the two bands are identical or the standard deviation of computed \code{VI} (for a given band combination) is zero, the correlation is set to 0,
#' thereby avoiding warnings.
#'
#' @seealso \code{\link{cor}}
#'
#' @examples
#' \dontrun{
#'   library(visa)
#'   data(NSpec.DF)
#'   X <- NSpec.DF$spectra[, seq(1, ncol(NSpec.DF$spectra), 5)]  # resampled to 5 nm steps
#'   y <- NSpec.DF$N  # nitrogen
#'   cm <- cm.nsr(X, y, cm.plot = TRUE)
#' }
#'
#' @import ggplot2 Matrix reshape2 grDevices
#' @importFrom stats sd
#' @export
cm.nsr <- function(S, x, w = wavelength(S), w.unit = NULL, cm.plot = FALSE){

  # Determine the format of spectra
  spectra <- spectra(S)
  if (is.matrix(spectra) && is.null(colnames(spectra)) && length(w) == 0)
    stop("Wavelength for the spectra matrix is not correctly defined")

  n <- dim(spectra)[2]  # Number of wavebands, should equal length(w)

  ## Compute (Rj-Ri)/(Rj+Ri) for all band combinations
  R2 <- Matrix::Matrix(0, n, n, sparse = TRUE)  # Zero sparse matrix
  Rj <- spectra
  ones <- matrix(1, 1, n)

  for (cI in 1:n) {
    Ri <- spectra[, cI]
    Ri <- Ri %*% ones  # Convert to matrix (each column is Ri)
    # Calculate V for each band (column)
    V <- (Rj - Ri) / (Rj + Ri)
    # Compute correlation for each column in V, avoiding warning if sd is zero
    rvals <- apply(V, 2, function(v) {
      # Bugfix: adding isTRUE() to fix sd() returns NaN
      if (isTRUE(sd(v)==0)) 0 else stats::cor(x, v)
    })
    Rcorr2 <- rvals^2
    # Store the squared correlations in the corresponding column of R2
    spR2 <- Matrix::sparseMatrix(i = rep(cI, n),
                                j = 1:n,
                                x = as.numeric(Rcorr2),
                                dims = c(n, n))
    R2 <- R2 + spR2
  }

  #row_names <- paste(w, "nm")
  #col_names <- paste(w, "nm")
  #cm <- as.matrix(R2, dimnames = list(row_names, col_names)) # failed to save names to cm!

  cm <- as.matrix(R2)
  rownames(cm) <- w # 2025-03-16 specify row/col names to return cm with row/column names
  colnames(cm) <- w # 2025-03-16 specify row/col names to return cm with row/column names

  R2max <- max(cm, na.rm = TRUE)
  print(paste('The max value of R^2 is', as.character(round(R2max, 4))))
  ind_max <- which(cm == R2max, arr.ind = TRUE)
  bestBands <- w[ind_max[1, ]]
  print(paste(c("i", "j"), as.vector(bestBands), sep = "_"))

  if (isTRUE(cm.plot)) {
    cm_plot <- plt.2dcm(cm, show.stat = FALSE)
    print(cm_plot)
  }

  cm
}

