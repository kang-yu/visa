#' Create a ggplot Plot for Linear Fit with Equation and R²
#'
#' This function plots a linear model fit using ggplot2. It creates a scatter plot with a regression line,
#' and displays the regression equation along with the R² value.
#'
#' When provided with two numeric vectors, the function treats them as x and y values, respectively,
#' constructs a data frame, and applies a default mapping. Alternatively, if a data frame is provided,
#' an aesthetic mapping (or default mapping) will be used.
#'
#' @rdname ggplot-method
#'
#' @param data Either a numeric vector (to be used as x) or an object containing the data (e.g., a data frame).
#' @param mapping Either a numeric vector (to be used as y when data is numeric) or an aesthetic mapping
#'   created with \code{ggplot2::aes()}. If \code{mapping} is missing and \code{data} is a data frame,
#'   the default mapping \code{aes(x, y)} is used.
#' @param ... Other arguments passed to ggplot2 components.
#' @param environment The environment in which to evaluate the plot. Defaults to \code{parent.frame()}.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#'   library(visa)
#'   # Using numeric vectors for x and y:
#'   ggplot.lmfit(1:10, 2:11)
#'
#'   # Using a data frame:
#'   df <- data.frame(x = runif(10, 1, 10), y = runif(10, 2, 11) + 0.5)
#'   ggplot.lmfit(df, aes(x, y))
#' }
#'
#' @import ggplot2
#' @importFrom ggpmisc stat_poly_eq
#' @export
#' @method ggplot lmfit
ggplot.lmfit <- function(data, mapping = NULL, ..., environment = parent.frame()){

  # If both 'data' and 'mapping' are numeric vectors, treat them as x and y.
  if (is.numeric(data) && is.numeric(mapping)) {
    df <- data.frame(x = data, y = mapping)
    mapping <- ggplot2::aes(x = x, y = y)
  } else {
    # Otherwise, assume 'data' is an object that can be coerced to a data frame.
    if (!is.data.frame(data)) {
      data <- as.data.frame(data)
    }
    df <- data
    if (is.null(mapping)) {
      mapping <- ggplot2::aes(x = x, y = y)
    }
  }

  # Define the formula for the linear fit.
  my.formula <- y ~ x

  # Create the ggplot object with points, a linear regression line, and the regression equation with R².
  p <- ggplot2::ggplot(data = df, mapping = mapping, ..., environment = environment) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "blue", formula = my.formula) +
    ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label),
                                            sep = "*plain(\",\")~")),
                          formula = my.formula, rr.digits = 4,
                          parse = TRUE, col = "blue", size = 4, ...)

  p
}
