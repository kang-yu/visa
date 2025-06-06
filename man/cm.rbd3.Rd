% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cm.rbd3.R
\name{cm.rbd3}
\alias{cm.rbd3}
\title{Calculate 3-Band Correlation Array for Spectral Data correlating with another variable x}
\usage{
cm.rbd3(
  S,
  x,
  w = wavelength(S),
  w.unit = NULL,
  cm.plot = FALSE,
  plot.method = "default"
)
}
\arguments{
\item{S}{A spectral data object or matrix. Each column corresponds to a spectral band.}

\item{x}{A numeric vector representing the response variable (e.g., chlorophyll).}

\item{w}{A numeric vector of wavelengths; by default, it is derived using \code{wavelength(S)}.}

\item{w.unit}{Character string specifying the unit of wavelengths (optional).}

\item{cm.plot}{Logical. If \code{TRUE}, a 3D slice plot of the correlation array is generated.}

\item{plot.method}{Character string specifying the plotting method. Currently, the plotting option uses \code{plot3D}.}
}
\value{
A 3-dimensional array of squared correlation (\eqn{R^2}) values with dimensions corresponding to the
combinations of bands \eqn{i}, \eqn{j}, and \eqn{k}.
}
\description{
This function computes the squared Pearson correlation (\eqn{R^2}) between a response vector \code{x}
and a derived variable \eqn{V} for every possible combination of three distinct spectral bands. The derived variable \eqn{V}
is calculated using the formula:
\deqn{V = \frac{R_k - R_j}{R_j - R_i}}
where \eqn{R_i}, \eqn{R_j}, and \eqn{R_k} represent the reflectance values at bands \eqn{i}, \eqn{j}, and \eqn{k}, respectively.
}
\details{
The function prints the maximum \eqn{R^2} value and the corresponding band wavelengths. Optionally, it can produce a 3D slice plot
of the correlation array using \code{plot3D::slice3D}.


For every combination of three distinct bands (\eqn{i}, \eqn{j}, \eqn{k}), the function computes
\deqn{V = \frac{R_k - R_j}{R_j - R_i}}
and then calculates the squared Pearson correlation between \code{x} and \code{V}.
The maximum \eqn{R^2} value and its associated band combination are printed.

If \code{cm.plot} is set to \code{TRUE}, the function generates a 3D slice plot of the correlation array using the best band combination,
where the slices correspond to the wavelengths of the bands.
}
\examples{
\dontrun{
  library(visa)
  data(NSpec.DF)
  x <- NSpec.DF$N # nitrogen
  # Below resamples spectra to 20 nm for fast computation
  S <- NSpec.DF$spectra[, seq(1, ncol(NSpec.DF$spectra), 20)]
  # S is a spectral data object and x is a numeric vector.
  Rsq3 <- cm.rbd3(S, x, cm.plot = TRUE)
}

}
