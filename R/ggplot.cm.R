#' Create a ggplot Plot from Correlation Coefficients
#'
#' This function creates a ggplot visualization from a correlation matrix or array.
#' It works for both 2D correlation matrices (e.g., from \code{cm.sr} or \code{cm.nsr})
#' and 3D correlation arrays (e.g., from \code{cm.rbd3}). For 2D matrices, a single heatmap
#' is produced. For 3D arrays, a faceted heatmap is produced for each slice along the third dimension (\code{k}).
#'
#' @rdname ggplot.cm
#'
#' @param data A numeric 2D matrix or 3D array of correlation coefficients.
#' @param mapping Optional ggplot2 aesthetic mapping.
#' @param ... Additional arguments passed to ggplot.
#' @param show.stat Logical. If \code{TRUE}, prints the best R\eqn{^2} value and corresponding bands.
#' @param environment The environment in which to evaluate the plot. Defaults to \code{parent.frame()}.
#'
#' @return
#'   \item{cm_plot}{A ggplot object visualizing the correlation matrix (or matrices).}
#'
#' @details
#' For a 2D matrix, the function extracts wavelengths from the column names (assuming they are numeric) and produces a heatmap.
#' For a 3D array, it assumes the dimensions correspond to bands (i, j, k) and produces a faceted heatmap with each facet
#' representing a slice for a given value of k.
#'
#' @examples
#' \dontrun{
#'   library(visa)
#'   data(NSpec.DF)
#'   x <- NSpec.DF$N # nitrogen
#'   S <- NSpec.DF$spectra[, seq(1, ncol(NSpec.DF$spectra), 5)] # resampled to 10 nm steps
#'
#'   # 2D correlation matrix example (from cm.sr or cm.nsr):
#'   cm2d <- cm.sr(S, x, cm.plot = FALSE)
#'   p2d <- ggplot.cm(cm2d)
#'   print(p2d)
#'
#'   # 3D correlation array example (from cm.rbd3):
#'   cm3d <- cm.rbd3(S, x, cm.plot = FALSE)
#'   p3d <- ggplot.cm(cm3d)
#'   print(p3d)
#' }
#'
#' @import ggplot2 reshape2 grDevices RColorBrewer
#' @export ggplot.cm
ggplot.cm <- function(data, mapping = NULL, ...,
                      show.stat = TRUE,
                      environment = parent.frame()){

  dims <- dim(data)

  if (length(dims) == 2) {
    # ----- 2D correlation matrix case -----
    # Try extracting wavelengths from column names (assumes they contain numeric info)
    w <- as.numeric(gsub("\\D", "", colnames(data)))
    if (all(is.na(w))) {
      warning("Column names could not be converted to numeric wavelengths; using sequential indices.")
      w <- 1:ncol(data)
    } else {
      # Replace any NA values with sequential indices
      w[is.na(w)] <- seq_along(w)[is.na(w)]
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
    w1_index <- as.numeric(as.character(cmDF$Var1))
    w2_index <- as.numeric(as.character(cmDF$Var2))
    # If conversion fails, use sequential indices
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
    # ----- 3D correlation array case -----
    # Assume dimensions order is (i, j, k)
    if (is.null(dimnames(data))) {
      dimnames(data) <- list(i = as.character(1:dims[1]),
                             j = as.character(1:dims[2]),
                             k = as.character(1:dims[3]))
    }

    # Convert dimnames to numeric wavelengths, if possible.
    w_i <- as.numeric(dimnames(data)$i)
    w_j <- as.numeric(dimnames(data)$j)
    w_k <- as.numeric(dimnames(data)$k)
    if (any(is.na(w_i))) w_i <- 1:dims[1]
    if (any(is.na(w_j))) w_j <- 1:dims[2]
    if (any(is.na(w_k))) w_k <- 1:dims[3]

    R2max <- max(data, na.rm = TRUE)
    if (show.stat)
      print(paste('The max value of R^2 is', round(R2max, 4)))
    ind_max <- which(data == R2max, arr.ind = TRUE)
    bestBands <- c(w_i[ind_max[1, "i"]],
                   w_j[ind_max[1, "j"]],
                   w_k[ind_max[1, "k"]])
    if (show.stat)
      print(paste("Best band combination (i, j, k):", paste(bestBands, collapse = ", ")))

    dataDF <- as.data.frame(as.table(data))
    colnames(dataDF) <- c("i", "j", "k", "value")
    dataDF$i <- w_i[as.numeric(as.character(dataDF$i))]
    dataDF$j <- w_j[as.numeric(as.character(dataDF$j))]
    dataDF$k <- w_k[as.numeric(as.character(dataDF$k))]

    myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")), space = "Lab")

    cmp <- ggplot2::ggplot(dataDF, aes(x = i, y = j, fill = value)) +
      geom_tile() +
      scale_fill_gradientn(colours = myPalette(100)) +
      coord_equal() +
      theme_bw() +
      facet_wrap(~ k, labeller = label_both) +
      xlab("Band i") +
      ylab("Band j") +
      ggtitle("3D Correlation Matrix (slices by Band k)")
    return(cmp)

  } else {
    stop("Input data must be either a 2D matrix or a 3D array.")
  }
}
