#' Plot Model Fit with Equation and R^2.
#'
#' This functions plots model fit using ggplot.
#'
#' Visualization of linear fit (y = ax + b), using scatter plots and with regression line, as well as
#' added details of regression equation and R^2.
#'
#' @rdname plot-method
#' @aliases plot.fit
#' @param x A numeric variable
#' @param y A numeric variable
#' @examples
#' \dontrun{
#' library(visa)
#' x <- 1:10
#' y <- 2:11+0.5
#' plot.fit(x,y)
#' }
#' @import ggplot2 ggpmisc
#' @export plot.lmfit

plot.lmfit <- function(x,y){

  df <- data.frame(x,y)
  my.formula <- y ~ x
  p <- ggplot2::ggplot(data = df, aes(x = x, y = y)) +
    geom_smooth(method = "lm", se = FALSE, color = "blue", formula = my.formula) +
    geom_point()

  yrange <- ggplot_build(p)$panel$ranges[[1]]$y.range
  xrange <- ggplot_build(p)$panel$ranges[[1]]$x.range

  p <- p +
    stat_poly_eq(formula = my.formula, eq.with.lhs = "italic(y)~`=`~",
                 aes(label = paste(stat(eq.label), stat(rr.label), sep = "~~~")),
                 parse = TRUE, col = "blue", label.x = xrange[2]*0.5,
                 label.y = yrange[2]*0.95, size = 4)
  p
}
