#' Create a ggplot plot for linear fit with Equation and R^2.
#'
#' This functions plots model fit using ggplot.
#'
#' Visualization of linear fit (y = ax + b), using scatter plots and with regression line, as well as
#' added details of regression equation and R^2.
#'
#' @rdname ggplot-method
#'
#' @param x,y A dataframe
#' @param ... Other arguments passed on to methods. Not currently used.
#' @param environment If an variable defined in the aesthetic mapping is not
#' found in the data, ggplot will look for it in this environment. It defaults
#' to using the environment in which \code{ggplot()} is called.
#'
#' @examples
#' \dontrun{
#' library(visa)
#' x <- 1:10
#' y <- 2:11+0.5
#' ggplot.lmfit(x, y)
#' }
#' @import ggplot2 ggpmisc
#' @importFrom rlang .data
#' @export ggplot.lmfit

ggplot.lmfit <- function(x, y,
                         ...,
                         environment = parent.frame()){

  df <- data.frame(x,y)
  my.formula <- y ~ x
  p <- ggplot2::ggplot(data = df, aes(x, y),
                       ... = ...,
                       environment = environment) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue", formula = my.formula) +
    stat_poly_eq(aes(label = paste(stat(eq.label), stat(rr.label), sep = "~~~")),
                 formula = my.formula, rr.digits = 4,
                 parse = TRUE, col = "blue", size = 4)

  # yrange <- ggplot_build(p)$panel$ranges[[1]]$y.range
  # xrange <- ggplot_build(p)$panel$ranges[[1]]$x.range

  # p <- p + stat_poly_eq(data = df, formula = my.formula, eq.with.lhs = "italic(y)~`=`~",
  #                       aes(label = paste(stat(eq.label), stat(rr.label), sep = "~~~")),
  #                       parse = TRUE, col = "blue", label.x = xrange[2]*0.5,
  #                       label.y = yrange[2]*0.95, size = 4)

  p
}
