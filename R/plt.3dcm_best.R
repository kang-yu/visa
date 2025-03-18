#' Create a 3D Slice Plot of Correlation Array
#'
#' This function creates an interactive Plotly 3D slice plot from a 3D correlation array.
#' The function uses the array's dimnames to define the coordinate values. If no dimnames are present,
#' a warning is issued and sequential indices are used. The plot displays three surfaces corresponding
#' to slices along each dimension (i, j, k) at the best band combination (where the correlation value is maximal).
#'
#' @param R3 A 3D numeric array of correlation coefficients.
#' @param colorscale A character string specifying the colorscale to use. If \code{"Spectral"}, a custom
#'   colorscale is created using \code{colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")), space = "Lab")(100)}.
#'   Otherwise, the provided value is used (default is \code{"Spectral"}).
#'
#' @return An interactive Plotly figure showing three surfaces corresponding to constant slices along dimensions i, j, and k.
#'
#' @details The function first checks if the input 3D array has proper dimnames. If not, it issues a warning
#' and assigns sequential indices as dimnames. It then melts the array to find the best band combination based on the
#' maximum correlation value. Using the dimnames, it finds the numeric indices for the best bands and creates grid matrices
#' for each slice. A custom colorscale is built when \code{colorscale = "Spectral"}, and the Plotly figure is constructed
#' with a single color bar.
#'
#' @examples
#' \dontrun{
#'   # Assume cm3d is a 3D correlation array with proper dimnames.
#'   plt.3dcm_best(cm3d)
#' }
#'
#' @import grDevices RColorBrewer
#' @importFrom plotly plot_ly add_surface layout
#' @importFrom magrittr %>%
#' @export
plt.3dcm_best <- function(R3, colorscale = "Spectral"){

  dims <- dim(R3)
  dn <- dimnames(R3)

  # Check if the 3D array has proper dimnames.
  if (is.null(dn) || any(sapply(dn, is.null))) {
    warning("3D array has no dimnames; please assign them. Using sequential indices instead.")
    dn <- list(i = as.character(1:dims[1]),
               j = as.character(1:dims[2]),
               k = as.character(1:dims[3]))
    dimnames(R3) <- dn
  }

  # If the dimnames list does not have names, assign default names.
  if (is.null(names(dn))) {
    names(dn) <- c("i", "j", "k")
    dimnames(R3) <- dn
  }

  # Melt the array to a data frame. This will automatically use the existing dimnames.
  R3_df <- as.data.frame(as.table(R3))
  if (!all(c("i", "j", "k") %in% colnames(R3_df))) {
    colnames(R3_df)[1:3] <- c("i", "j", "k")
  }
  colnames(R3_df)[4] <- "val"

  # Determine the best slice based on maximum correlation value.
  max_val <- max(R3_df$val, na.rm = TRUE)
  best_idx <- which(R3_df$val == max_val)[1]
  bestBands <- c(R3_df$i[best_idx], R3_df$j[best_idx], R3_df$k[best_idx])

  # Using the dimnames, find the numeric positions of the best bands.
  slice_x <- match(bestBands[1], dn[[1]])
  slice_j <- match(bestBands[2], dn[[2]])
  slice_k <- match(bestBands[3], dn[[3]])

  cat("Best band combination (i, j, k):", paste(bestBands, collapse = ", "), "\n")

  # Create grid matrices for each slice.
  # For slice at constant i (slice_x): extract R3[slice_x, , ] (dimensions: j x k)
  surface_i <- R3[slice_x, , ]
  grid_j <- matrix(as.numeric(dn[[2]]), nrow = dims[2], ncol = dims[3], byrow = FALSE)
  grid_k <- matrix(as.numeric(dn[[3]]), nrow = dims[2], ncol = dims[3], byrow = TRUE)
  grid_i <- matrix(as.numeric(dn[[1]][slice_x]), nrow = dims[2], ncol = dims[3])

  # For slice at constant j (slice_j): extract R3[, slice_j, ] (dimensions: i x k)
  surface_j <- R3[, slice_j, ]
  grid_i2 <- matrix(as.numeric(dn[[1]]), nrow = dims[1], ncol = dims[3], byrow = FALSE)
  grid_k2 <- matrix(as.numeric(dn[[3]]), nrow = dims[1], ncol = dims[3], byrow = TRUE)
  grid_j2 <- matrix(as.numeric(dn[[2]][slice_j]), nrow = dims[1], ncol = dims[3])

  # For slice at constant k (slice_k): extract R3[, , slice_k] (dimensions: i x j)
  surface_k <- R3[, , slice_k]
  grid_i3 <- matrix(as.numeric(dn[[1]]), nrow = dims[1], ncol = dims[2], byrow = FALSE)
  grid_j3 <- matrix(as.numeric(dn[[2]]), nrow = dims[1], ncol = dims[2], byrow = TRUE)
  grid_k3 <- matrix(as.numeric(dn[[3]][slice_k]), nrow = dims[1], ncol = dims[2])

  # Create a custom colorscale as a list of stops.
  if (colorscale == "Spectral") {
    col_vec <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")),space = "Lab")(100)
    colorscale_val <- lapply(seq_along(col_vec) - 1, function(i) {
      list(i / (length(col_vec) - 1), col_vec[i + 1])
    })
  } else {
    colorscale_val <- "Jet"
  }

  # Build the Plotly figure with three surfaces.
  fig <- plotly::plot_ly() %>%
    add_surface(x = grid_i, y = grid_j, z = grid_k,
                surfacecolor = surface_i,
                colorscale = colorscale_val,
                colorbar = list(
                  x = 1.05,         # move the colorbar to the right of the plot
                  y = 0.5,          # center it vertically
                  yanchor = "middle",
                  title = "R^2"
                ),
                showscale = TRUE,
                name = paste("Slice at i =", bestBands[1])) %>%
    add_surface(x = grid_i2, y = grid_j2, z = grid_k2,
                surfacecolor = surface_j,
                colorscale = colorscale_val,
                showscale = FALSE,
                name = paste("Slice at j =", bestBands[2])) %>%
    add_surface(x = grid_i3, y = grid_j3, z = grid_k3,
                surfacecolor = surface_k,
                colorscale = colorscale_val,
                showscale = FALSE,
                name = paste("Slice at k =", bestBands[3])) %>%
    layout(title = "3D Slice Plot of Correlation Array",
           scene = list(
             xaxis = list(title = "Band i"),
             yaxis = list(title = "Band j"),
             zaxis = list(title = "Band k")
           ))

  print(fig)
}
