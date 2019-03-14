#' Calculate simple ratio (SR) indices using two spectral bands.
#'
#' descriptions
#'
#' details
#'
#' @param s Spectral data in the format of visa's Spectra object, spectra.data.frame or spectra.matrix.
#' @param b1 A integer number which defines the wavelength of the 1st spectral band.
#' @param b2 A integer number which defines the wavelength of the 2nd spectral band.
#' @examples
#'   \donotrun{
#' library(visa)
#' x <- NSpec.DF$N
#' s <- NSpec.DF$Spec
#' sr1 <- sr(s, 440, 445)
#' }
#' @import ggplot2 ggpmisc
#' @export

sr <- function(s, b1, b2){
  spec <- spectra(s)
  wl <- wavelength(s)
  idx1 <- b1 %in% wl
  idx2 <- b2 %in% wl
  s1 <- spec[, idx1]
  s2 <- spec[, idx2]
  sr <- s1/s2
}

#' Calculate Normalized simple Ratio (NSR) index using two spectral bands.
#'
#' descriptions
#'
#' @rdname sr
#' @inheritParams sr
#' @export
nsr <- function(s, b1, b2){
  sr <- sr(s,b1,b2)
  nsr <- (sr - 1)/(sr + 1)
}



ndvi2 <- function(x,y,w,p){

  bandInd <- which(w %in% p)

  bestNDVI <- (x[,bandInd[2]] - x[,bandInd[1]]) / (x[,bandInd[2]] + x[,bandInd[1]])
  # r <- cor(bestNDVI,y)

  x2 <- bestNDVI
  df <- data.frame(x2,y)

  my.formula <- y ~ x
  p <- ggplot2::ggplot(data = df, aes(x = x2, y = y)) +
    geom_smooth(method = "lm", se = FALSE, color = "blue",formula = my.formula) +
    geom_point()

  yrange <- ggplot_build(p)$panel$ranges[[1]]$y.range
  xrange <- ggplot_build(p)$panel$ranges[[1]]$x.range
  bstr <- paste0("[", paste(p, collapse = ","), "]")

  p <- p + stat_poly_eq(formula = my.formula, eq.with.lhs = "italic(hat(y))~`=`~",
                        aes(label = paste(stat(eq.label), stat(rr.label), sep = "~~~")),
                        parse = TRUE, col = "blue", label.x = xrange[2]*0.5, label.y = yrange[2]*0.95, size = 5)+
    labs(x = bquote(NDVI[parse(bstr)]))

  p

}

