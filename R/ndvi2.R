#' Calculate and plot the two NDVI and best NDVI calculated using \code{\link{ndvi}}
#'
#' @rdname ndvi2
#' @param x A matrix of spectral data, a row is a spectrum across all spectral bands.
#' @param y A vector.
#' @param w A vector of wavelength.
#' @param p A vector of two elements (the pair of bands used for NDVI calculation).
#' @details
#' This function plots the best NDVI against the y
#' @examples
#'   \donotrun{
#' library(visa)
#' y <- NSpec.DF$N
#' x <- NSpec.DF$Spec
#' w <- as.numeric(gsub("nm", "", colnames(NSpec.DF$Spec)))
#' p <- c(440,445)
#' ndvi2(x,y,w,p)
#' wavelength(spectra)
#' }
#' @import ggplot2 ggpmisc
#' @export

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

