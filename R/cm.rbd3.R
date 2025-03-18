#' Calculate 3-Band Correlation Array for Spectral Data correlating with another variable x
#'
#' This function computes the squared Pearson correlation (\eqn{R^2}) between a response vector \code{x}
#' and a derived variable \eqn{V} for every possible combination of three distinct spectral bands. The derived variable \eqn{V}
#' is calculated using the formula:
#' \deqn{V = \frac{R_k - R_j}{R_j - R_i}}
#' where \eqn{R_i}, \eqn{R_j}, and \eqn{R_k} represent the reflectance values at bands \eqn{i}, \eqn{j}, and \eqn{k}, respectively.
#'
#' The function prints the maximum \eqn{R^2} value and the corresponding band wavelengths. Optionally, it can produce a 3D slice plot
#' of the correlation array using \code{plot3D::slice3D}.
#'
#' @param S A spectral data object or matrix. Each column corresponds to a spectral band.
#' @param x A numeric vector representing the response variable (e.g., chlorophyll).
#' @param w A numeric vector of wavelengths; by default, it is derived using \code{wavelength(S)}.
#' @param w.unit Character string specifying the unit of wavelengths (optional).
#' @param cm.plot Logical. If \code{TRUE}, a 3D slice plot of the correlation array is generated.
#' @param plot.method Character string specifying the plotting method. Currently, the plotting option uses \code{plot3D}.
#'
#' @return A 3-dimensional array of squared correlation (\eqn{R^2}) values with dimensions corresponding to the
#' combinations of bands \eqn{i}, \eqn{j}, and \eqn{k}.
#'
#' @details
#' For every combination of three distinct bands (\eqn{i}, \eqn{j}, \eqn{k}), the function computes
#' \deqn{V = \frac{R_k - R_j}{R_j - R_i}}
#' and then calculates the squared Pearson correlation between \code{x} and \code{V}.
#' The maximum \eqn{R^2} value and its associated band combination are printed.
#'
#' If \code{cm.plot} is set to \code{TRUE}, the function generates a 3D slice plot of the correlation array using the best band combination,
#' where the slices correspond to the wavelengths of the bands.
#'
#' @examples
#' \dontrun{
#'   library(visa)
#'   data(NSpec.DF)
#'   x <- NSpec.DF$N # nitrogen
#'   # Below resamples spectra to 20 nm for fast computation
#'   S <- NSpec.DF$spectra[, seq(1, ncol(NSpec.DF$spectra), 20)]
#'   # S is a spectral data object and x is a numeric vector.
#'   Rsq3 <- cm.rbd3(S, x, cm.plot = TRUE)
#' }
#'
#' @import plot3D
#' @export
cm.rbd3 <- function(S, x, w = wavelength(S),
                    w.unit = NULL, cm.plot = FALSE,
                    plot.method = "default") {

  # Extract spectral data
  spectra <- spectra(S)
  if (is.matrix(spectra) && is.null(colnames(spectra)) && length(w) == 0)
    stop("Wavelength for the spectra matrix is not correctly defined")

  n <- ncol(spectra)  # Number of bands

  # Initialize a 3D array to store the squared correlation values.
  # Dimensions: first index = band i, second index = band j, third index = band k.
  R3 <- array(NA, dim = c(n, n, n))

  # Loop over all possible three-band combinations with distinct indices.
  for (j in 1:n) {
    Rk <- spectra[, j]
    for (i in 1:n) {
      if (i == j) next  # Ensure band i is not the same as band j.
      Ri <- spectra[, i]
      for (k in 1:n) {
        if (k == j || k == i) next  # Ensure band k is distinct from both j and i.
        Rj <- spectra[, k]

        # Compute V using the formula (Rk - Rj) / (Rj - Ri)
        V <- (Rk - Rj) / (Rj - Ri)
        # Compute the squared Pearson correlation between x and V.
        Rcorr <- (stats::cor(x, V))^2
        R3[i, j, k] <- Rcorr
      }
    }
  }

  # Identify the maximum correlation squared value.
  R3max <- max(R3, na.rm = TRUE)
  cat("The max value of R^2 is", round(R3max, 4), "\n")

  # Retrieve the indices (i, j, k) corresponding to the maximum value.
  ind_max <- which(R3 == R3max, arr.ind = TRUE)
  bestBands <- w[ind_max[1, ]]  # bestBands in order: (i, j, k)
  cat("Best band combination (i, j, k):", paste(bestBands, collapse = ", "), "\n")

  #########################################################
  # put the wavelengths names in the 3D dimension names
  dimnames(R3) <- list(i = w, j = w, k = w)

  if (isTRUE(cm.plot)) {
    cat("plot3D correlation array.\n")
    slice_x <- bestBands[1]  # slice for first band i
    slice_y <- bestBands[2]  # slice for second band j
    slice_z <- bestBands[3]  # slice for third band k

    visaColors <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")),
                                 space = "Lab")(100)

    # "3D Slice Plot of (Rk-Rj)/(Rj-Ri) Correlation with Chlorophyll"
    plot3D::slice3D(x = w, y = w, z = w,
                    vol = R3, colvar = R3,
                    col = visaColors,
                    xs = slice_x, ys = slice_y, zs = slice_z,
                    xlab = "Band i",
                    ylab = "Band j",
                    zlab = "Band k",
                    ticktype = "detailed")
  }

  return(R3)
}

