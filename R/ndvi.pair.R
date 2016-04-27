#' Plotting the best NDVI calculated using \code{\link{ndvi}}
#'
#' @param x A matrix of spectral data, a row is a spectrum across all spectral bands.
#' @param y A vector.
#' @param w A vector of wavelength.
#' @param p A vector of two elements (the pair of bands used for NDVI calculation).
#' @details
#' This function plots the best NDVI against the y
#' @examples
#' y <- exampleData[-1,1]
#' x <- exampleData[-1,-1]
#' w <- exampleData[1,-1]
#' p <- c(440,444)
#' ndvi.pair(x,y,w,p)
#' @export

ndvi.pair <- function(x,y,w,p){

  bandInd <- which(w %in% p)

  bestNDVI <- (x[,bandInd[2]] - x[,bandInd[1]]) / (x[,bandInd[2]] + x[,bandInd[1]])
  cor(bestNDVI,y)


  library(ggplot2)
  library(ggpmisc)

  x2 <- bestNDVI
  df <- data.frame(x2,y)

  my.formula <- y ~ x
  p <- ggplot(data = df, aes(x = x2, y = y)) +
    geom_smooth(method = "lm", se = FALSE, color = "blue",formula = my.formula) + geom_point()


  yrange <- ggplot_build(p)$panel$ranges[[1]]$y.range
  xrange <- ggplot_build(p)$panel$ranges[[1]]$x.range

  p <- p + stat_poly_eq(formula = my.formula, eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE, col="blue", label.x = xrange[2]*0.5, label.y = yrange[2]*0.95, size = 5)
  p

}


#' Say Hello!
view.datastr <- function() {
  str(c(x,y,w))
  print("Thansk for using this package!")
}
