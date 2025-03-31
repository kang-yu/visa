#' Selecting the best 2-Band combinations for Simple Ratio (SR)
#'
#' @name cm.sr
#' @description This function develops an optimization algorithm based on correlation analysis between the spectral matrix \code{spectra} and the
#' vegetation variable of interest \code{x}. It determines the best spectral band combinations of the full spectrum that are most predictive for \code{x}.
#'
#' @details This function runs a calculation of \deqn{ SR = \lambda_i / \lambda_j } using the spectra data for all the possible pairs/combinations of any two bands (i, j)
#' within the full spectrum range. Next, correlation analysis is then performed between all possible SR values and another vector variable \code{y}, and it returns
#' the squared Pearson correlation (\eqn{R^2}) which indicates the predictive performance of each SR and its corresponding two-band combination. The
#' output is the wavelength (nm) indicating the best two bands that produce the highest value of \eqn{R^2}.
#'
#' @inheritParams cm.nsr
#' @return
#'   \item{cm}{Returns a correlation coefficients matrix.}
#' @seealso \code{\link{cm.nsr}}
#' @examples
#' \dontrun{
#'   library(visa)
#'   data(NSpec.DF)
#'   # Using the example spectra matrix of the spectra.dataframe
#'   X <- NSpec.DF$spectra[, seq(1, ncol(NSpec.DF$spectra), 10)]  # resampled to 10 nm steps
#'   y <- NSpec.DF$N  # nitrogen
#'   cm <- cm.sr(X, y, cm.plot = FALSE)
#' }
#'
#' @import ggplot2 Matrix reshape2 grDevices
#' @importFrom stats sd
#' @export
cm.sr <- function(S, x, w = wavelength(S), w.unit = NULL, cm.plot = FALSE){

  # Account for the format of spectra
  spectra <- spectra(S)
  if (is.matrix(spectra) && is.null(colnames(spectra)) && length(w) == 0)
    stop("Wavelength for the spectra matrix is not correctly defined")

  n <- dim(spectra)[2]  # Number of wavebands, should equal length(w)

  dn <- list(paste0("i_", w), paste0("j_", w))

  ## Compute SR for each combination: SR = Ri/Rj

  R2 <- Matrix::Matrix(0, n, n, dimnames = dn, sparse = TRUE)  # Zero sparse matrix
  Rj <- spectra
  ones <- matrix(1, 1, n)

  for (cI in 1:n) {
    Ri <- spectra[, cI] # take a column/band i
    Ri <- Ri %*% ones   # Convert to matrix (each column is Ri)
    # Calculate SR for each band j
    V <- Ri / Rj
    # Compute correlation for each column of V, avoiding division by zero in sd.
    Rvals <- apply(V, 2, function(v) {
      # Bugfix: adding isTRUE() to fix sd() returns NaN
      if (isTUE((sd(v)==0))) 0 else stats::cor(x, v)
    })
    Rcorr2 <- Rvals^2
    # Store the squared correlations in the corresponding column of R2
    spR2 <- Matrix::sparseMatrix(i = rep(cI, n),
                                j = 1:n,
                                x = as.numeric(Rcorr2),
                                dims = c(n, n),
                                dimnames = dn)
    R2 <- R2 + spR2
  }

  row_names <- w
  col_names <- w
  #cm <- as.matrix(R2, dimnames = list(row_names, col_names)) # base func failed to save names to cm!
  cm <- Matrix::as.matrix(R2, dimnames = list(row_names, col_names)) # Matrix package

  #cm <- as.matrix(R2)
  #rownames(cm) <- w # 2025-03-16 specify row/col names to return cm with row/column names
  #colnames(cm) <- w # 2025-03-16 specify row/col names to return cm with row/column names

  R2max <- max(cm, na.rm = TRUE)
  print(paste('The max value of R^2 is', as.character(round(R2max, 4))))
  ind_max <- which(cm == R2max, arr.ind = TRUE)
  bestBands <- w[ind_max[1,]]
  print(paste(c("i", "j"), as.vector(bestBands), sep = "_"))

  # Set column names to include wavelengths (in nm)
  #colnames(cm) <- paste(w, "nm")

  # Plot the correlation matrix if requested
  if (isTRUE(cm.plot)) {
    cm_plot <- plt.2dcm(cm, show.stat = FALSE)
    print(cm_plot)
  }

  cm
}
