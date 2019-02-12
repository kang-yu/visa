#' Selecting the best 2-Band combinations for Normalized Difference Vegetation Index (NDVI)
#'
#' This function developes a optimization algorithm based on correlation analysis between spectral parameter 'X' and y, which
#' determines the best spectral bands of the full spectrum that are most predictive for 'y'.
#'
#' @param X A matrix of spectral data, a row is a spectrum across all spectral bands.
#' @param y A vector.
#' @param w A vector of wavelength.
#' @param plot.cm A logic value for whether plotting the coefficient matrix or not, default FALSE.
#' @return
#'   \item{cm}{Returns a coorrelation coefficients matrix corresponding to each band combination for NDVI.}
#' @details
#' This function runs a calculation of \deqn{ NDVI = (\lambda_i - \lambda_j)/(\lambda_i + \lambda_j) } using all the possible pairs/combinations of any two bands (i,j)
#' within the full spectrum range thoroughly. A correlation analysis is then performed between the y and all possible NDVIs, and it calculates
#' the correlation coefficients (r) which indicates the predictive performance of each NDVI and its corresponding two-band combination. The
#' output is the wavelength (nm) indicating the best two bands that produce the highest value of r.
#' @seealso \code{\link{cor}}
#' @examples
#' library(visa)
#' y <- specDF$N # nitrogen
#' X <- specDF$Spec[, seq(1, ncol(specDF$Spec), 5)] # resampled to 5 nm steps
#' w <- gsub("nm", "", colnames(specDF$Spec))
#' w <- as.numeric(w)[seq(1, ncol(specDF$Spec), 5)] # resampled to 5 nm steps
#' ndvi(X,y,w)
#' @import ggplot2 Matrix reshape2 grDevices
#' @export


ndvi <- function(X,y,w, plot.cm = FALSE){

  n <- dim(X)[2] # Returns the Number of wavebands
  wavelength <- w

  ## (Rj-Ri)/(Rj+Ri)

  R2 <- Matrix::Matrix(0, n, n, sparse = TRUE)  # Zero sparse matrix
  Rj <- X

  ones <- matrix(1,1,n)

  for (cI in 1:n){
    Ri <- X[,cI]
    Ri <- Ri %*% ones  # to matrix
    # VI formular
    V <- (Rj-Ri)/(Rj+Ri)

    # Squared values (R2) of the Pearson Correlation coefficients
    Rcorr <- (cor(y, V))^2
    # To store the value of R2
    spR <- Matrix::sparseMatrix(i = c(1:n),j = rep(cI,n), x = as.numeric(Rcorr), dims = c(n,n))
    R2 <- R2 + spR
  }

  max(R2, na.rm = TRUE)
  ZZ <- as.matrix(R2)

  # str(ZZ)
  max(ZZ, na.rm = TRUE)

  return(ZZ)

  #--------------------------------------------------------------------------
  # identify the max R2 and its corresponding bands
  #--------------------------------------------------------------------------

  R2max <- max(ZZ, na.rm = TRUE)
  print(paste('The max value of R^2 is', as.character(round(R2max,4))))

  ind_max <- which(ZZ == R2max, arr.ind = TRUE)
  ind_max

  bestBands = wavelength[ind_max[1,]]
  print(as.vector(bestBands))

  #----------------------------------
  # plot ndvi
  #----------------------------------

  ZZDF <- reshape2::melt(ZZ)

  w1_index <- ZZDF$Var1
  w2_index <- ZZDF$Var2
  ZZDF$Var1 <- wavelength[w1_index]
  ZZDF$Var2 <- wavelength[w2_index]

  myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")), space="Lab")

  zp1 <- ggplot(ZZDF, aes(Var1, Var2, fill = value)) # Var1+350-1
  zp1 <- zp1 + geom_tile()
  zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
  # zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
  # zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
  zp1 <- zp1 + coord_equal()
  zp1 <- zp1 + theme_bw()
  zp1 <- zp1 + xlab("Wavelength i") + ylab("Wavelength j")

  if (isTRUE(plot.cm))  print(zp1)

}

