#' Create a ggplot Plot from a 2D Correlation Matrix
#'
#' This function creates a ggplot visualization from a 2D correlation matrix,
#' such as those produced by \code{cm.sr} or \code{cm.nsr}. The function attempts
#' to extract numeric wavelengths from the column names. If the extraction fails,
#' sequential indices are used.
#'
#' It replaces the former \code{ggplot.cm()}.
#'
#' @rdname plt.2dcm
#'
#' @param data A numeric 2D matrix of correlation coefficients.
#' @param mapping Optional ggplot2 aesthetic mapping.
#' @param ... Additional arguments passed to \code{ggplot}.
#' @param show.stat Logical. If \code{TRUE}, prints the best \eqn{R^2} value and corresponding bands.
#' @param environment The environment in which to evaluate the plot. Defaults to \code{parent.frame()}.
#'
#' @return A ggplot object visualizing the correlation matrix.
#'
#' @details The function extracts numeric wavelengths from the column names of \code{data}.
#' If these cannot be determined, sequential indices are used instead.
#'
#' @examples
#' \dontrun{
#'   library(visa)
#'   data(NSpec.DF)
#'   x <- NSpec.DF$N  # nitrogen
#'   S <- NSpec.DF$spectra[, seq(1, ncol(NSpec.DF$spectra), 10)]  # resampled to 10 nm steps
#'   cm2d <- cm.sr(S, x, cm.plot = FALSE)
#'   p2d <- plt.2dcm(cm2d)
#'   print(p2d)
#' }
#'
#' @import ggplot2 reshape2 grDevices RColorBrewer
#' @export
plt.2dcm <- function(data, mapping = NULL, ...,
                      show.stat = TRUE,
                      environment = parent.frame()){

  dims <- dim(data)

  if (length(dims) != 2) {
    stop("Input data must be a 2D matrix.")
  }

  # Attempt to extract numeric wavelengths from column names
  col_names <- colnames(data)
  if (is.null(col_names)) {
    warning("No column names found; using sequential indices for wavelengths.")
    w <- 1:ncol(data)
  } else {
    w_raw <- gsub("\\D", "", col_names)
    w <- suppressWarnings(as.numeric(w_raw))
    if (any(is.na(w))) {
      warning("Some column names could not be converted to numeric wavelengths; using sequential indices for those columns.")
      w[is.na(w)] <- seq_along(w)[is.na(w)]
    }
    if (all(is.na(w))) {
      warning("Column names could not be converted to numeric wavelengths; using sequential indices.")
      w <- 1:ncol(data)
    }
  }

  R2max <- max(data, na.rm = TRUE)
  if (show.stat)
    print(paste('The max value of R^2 is', round(R2max, 4)))

  ind_max <- which(data == R2max, arr.ind = TRUE)
  bestBands <- w[ind_max[1,]]
  if (show.stat)
    print(paste(c("i", "j"), bestBands, sep = "_"))

  cmDF <- reshape2::melt(data)
  # Remove rows with missing values to avoid non-finite coordinates
  cmDF <- cmDF[!is.na(cmDF$value), ]

  # Replace factor indices with actual wavelengths
  # Var1 and Var2 have factor levels like "350 nm", "400 nm", etc.
  cmDF$Var1 <- as.numeric(gsub("[^0-9.]", "", as.character(cmDF$Var1)))
  cmDF$Var2 <- as.numeric(gsub("[^0-9.]", "", as.character(cmDF$Var2)))

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
}
