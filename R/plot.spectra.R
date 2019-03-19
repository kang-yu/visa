#' Plot Hyperspectral Data
#'
#' This function is to make a nice plot of hyperspectral data, e.g., ASD spectrometer,
#' by using the \code{matplot} function. It customizes x ticks and labels to allow a
#' better inspection on the spectral signials at each wavelength.
#'
#' @rdname plot-method
#'
#' @param S A matrix of spectral data.
#' @param w A numeric vector for wavelength.
#' @param type Inheritated from plot 'type'
#' @param xlab X label.
#' @param ylab Y label.
#' @param lwd linewidth
#' @param col color
#' @param xaxt plot function
#' @param show.xtick.text whether show x-ticks default true
#' @param xtick Define x-ticks labels, default seq(400,2500,100)
#' @param rug.interval Define the interval of rugs for wavelength.
#' @param \dots Arguments passed to \code{\link{matplot}}
#' @examples
#' \dontrun{
#' s <- spectra(NSpec.DB)
#' w <- wavelength(NSpec.DB)
#' plot.spectra(s, w)
#' }
#' @seealso \code{\link{rug}}
#' @importFrom graphics matplot axis par rug text
#' @export plot.spectra

plot.spectra <- function(S, w = wavelength(S), type = "l", xlab = NULL, ylab = NULL,
                         lwd = 1:3, col = 1:3, xaxt="n", show.xtick.text = TRUE,
                         xtick = seq(400,2500,100), rug.interval = 50, ...){
  # prepare data
  x <- w
  y <- t(spectra(S))

  matplot(x, y, type = type, xlab = xlab, ylab = ylab,
          lwd = lwd, col = col, xaxt="n", ...)

  axis(side=1, at=xtick, labels = FALSE)

  rug(x = seq(min(x), max(x), rug.interval), ticksize = 0.01, side = 1, lwd = 1)

  if (isTRUE(show.xtick.text)){
    text(x=xtick, par("usr")[3], labels = xtick, pos = 1, offset = 1, xpd = TRUE)
  }
}
