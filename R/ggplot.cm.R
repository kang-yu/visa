#' Create a ggplot Plot from Correlation Coefficients (Deprecated)
#'
#' **Deprecated:** This function is deprecated and will be removed in a future release.
#' Please use \code{plt.2dcm} for 2D correlation matrices or the appropriate new functions for 3D plots.
#'
#' This function creates a ggplot visualization from a 2D correlation matrix.
#' It attempts to extract numeric wavelengths from the column names of the input matrix.
#'
#' @rdname ggplot.cm
#'
#' @param data A numeric 2D matrix of correlation coefficients. (For 3D arrays, a warning is issued.)
#' @param mapping Optional ggplot2 aesthetic mapping.
#' @param ... Additional arguments passed to \code{ggplot}.
#' @param show.stat Logical. If \code{TRUE}, prints the best \eqn{R^2} value and corresponding bands.
#' @param environment The environment in which to evaluate the plot. Defaults to \code{parent.frame()}.
#'
#' @return A ggplot object visualizing the correlation matrix. For 3D arrays, returns \code{NULL}.
#'
#' @details This function extracts numeric wavelengths from the column names of \code{data}. If these
#' cannot be determined, sequential indices are used instead.
#'
#' @examples
#' \dontrun{
#'   library(visa)
#'   data(NSpec.DF)
#'   x <- NSpec.DF$N  # nitrogen
#'    # resampled to 10 nm steps
#'   S <- NSpec.DF$spectra[, seq(1, ncol(NSpec.DF$spectra), 10)]
#'   cm2d <- cm.sr(S, x, cm.plot = FALSE)
#'   p2d <- ggplot.cm(cm2d)
#'   print(p2d)
#'
#' }
#'
#' @import ggplot2 reshape2 grDevices RColorBrewer
#' @export
ggplot.cm <- function(data, mapping = NULL, ...,
                      show.stat = TRUE,
                      environment = parent.frame()){
  .Deprecated("plt.2dcm", package = "visa",
              msg = "ggplot.cm() is deprecated. Please use plt.2dcm() for 2D correlation plots.")

  dims <- dim(data)

  if (length(dims) == 2) {
    # ----- 2D correlation matrix case -----
    col_names <- colnames(data)
    w <- if (is.null(col_names)) {
      warning("No column names found; using sequential indices for wavelengths.")
      1:ncol(data)
    } else {
      w_raw <- gsub("\\D", "", col_names)
      w_num <- suppressWarnings(as.numeric(w_raw))
      if (any(is.na(w_num))) {
        warning("Some column names could not be converted to numeric wavelengths; using sequential indices for those columns.")
        w_num[is.na(w_num)] <- seq_along(w_raw)[is.na(w_num)]
      }
      if (all(is.na(w_num))) 1:ncol(data) else w_num
    }

    R2max <- max(data, na.rm = TRUE)
    if (show.stat)
      print(paste('The max value of R^2 is', round(R2max, 4)))

    ind_max <- which(data == R2max, arr.ind = TRUE)
    bestBands <- w[ind_max[1,]]
    if (show.stat)
      print(paste(c("i", "j"), bestBands, sep = "_"))

    cmDF <- reshape2::melt(data)
    cmDF <- cmDF[!is.na(cmDF$value), ]

    w1_index <- as.numeric(as.character(cmDF$Var1))
    w2_index <- as.numeric(as.character(cmDF$Var2))
    if (any(is.na(w1_index))) w1_index <- seq_along(w)[as.numeric(cmDF$Var1)]
    if (any(is.na(w2_index))) w2_index <- seq_along(w)[as.numeric(cmDF$Var2)]
    cmDF$Var1 <- w[w1_index]
    cmDF$Var2 <- w[w2_index]

    myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")), space = "Lab")

    if (is.null(mapping)) {
      mapping <- aes_string("Var1", "Var2", fill = "value")
    }

    cmp <- ggplot2::ggplot(cmDF, mapping = mapping,
                           ... = ...,
                           environment = environment) +
      geom_tile() +
      scale_fill_gradientn(colours = myPalette(100)) +
      coord_equal() +
      theme_bw() +
      xlab("Band i") +
      ylab("Band j")

    return(cmp)

  } else if (length(dims) == 3) {
    # 3D branch: this function is deprecated for 3D arrays.
    warning("Input data is a 3D array; ggplot.cm() is deprecated for 3D arrays. Please use the appropriate 3D plotting function.")
    return(NULL)

  } else {
    stop("Input data must be either a 2D matrix or a 3D array.")
  }
}
