#' Find Best Band Combinations
#'
#' This function identifies the best band combination from the \link{cm.nsr} or \link{cm.sr}
#' calculated correlation coefficients - a numeric matrix (2D), or the \link{cm.rbd3} returned array (3D)
#' by locating the maximum value in the input \code{R} and returning the corresponding wavelengths
#' from \code{w}. For a 2D matrix, it returns a two-element vector (i, j); for a 3D array, a three-element
#' vector (i, j, k).
#'
#' @param R A numeric matrix (2D) or array (3D) containing metric values (e.g., correlation values).
#' @param w A numeric vector of wavelengths corresponding to the bands.
#'
#' @return A vector of wavelengths corresponding to the best band combination.
#'
#' @details
#' The function first verifies that \code{R} has dimensions. It then computes the maximum value in \code{R},
#' retrieves the indices corresponding to that value, and extracts the wavelengths from \code{w} based on the
#' dimensionality of \code{R}. If \code{R} is 2D, the order is assumed to be (i, j); if \code{R} is 3D, the order
#' is (i, j, k).
#'
#' @examples
#' # Example for a 2D matrix:
#' R_mat <- matrix(c(0.2, 0.8, 0.5, 0.3), nrow = 2)
#' wavelengths <- c(450, 550)
#' bestBands <- find.bestBands(R_mat, wavelengths)
#'
#' # Example for a 3D array:
#' R_arr <- array(runif(27), dim = c(3, 3, 3))
#' wavelengths <- c(400, 450, 500)
#' bestBands <- find.bestBands(R_arr, wavelengths)
#'
#' @export
find.bestBands <- function(R, w){
  # Check if R has dimensions and compute the maximum value.
  if (is.null(dim(R))) {
    stop("Input R must be a matrix (2D) or array (3D).")
  }

  Rmax <- max(R, na.rm = TRUE)
  cat("The max value is", round(Rmax, 4), "\n")

  # Retrieve the indices corresponding to the maximum value.
  ind_max <- which(R == Rmax, arr.ind = TRUE)

  # Check the dimensionality of R and extract the best bands accordingly.
  if (length(dim(R)) == 3) {
    # For 3D array: assume order is (i, j, k)
    bestBands <- w[ind_max[1, ]]
    cat("Best band combination (i, j, k):", paste(bestBands, collapse = ", "), "\n")
  } else if (length(dim(R)) == 2) {
    # For 2D matrix: assume order is (i, j)
    bestBands <- w[ind_max[1, ]]
    cat("Best band combination (i, j):", paste(bestBands, collapse = ", "), "\n")
  } else {
    stop("Input R must be either a 2D matrix or a 3D array.")
  }

  return(bestBands)
}
