#' Selecting the best 2-Band combinations for Normalized Simple Ratio (NSR)
#'
#' This function developes a optimization algorithm based on correlation analysis between spectral matrix 'spectra' and the
#' getation variable of interest x, which
#' determines the best spectral band combinations of the full spectrum that are most predictive for 'x'.
#'
#' @param spectra A matrix of spectral data, a row is a spectrum across all spectral bands.
#' @param x A vector.
#' @param w A vector of wavelength.
#' @param cm.plot A logic value for whether plotting the coefficient matrix or not, default FALSE.
#' @return
#'   \item{cm}{Returns a coorrelation coefficients matrix corresponding to each band combination for NDVI.}
#' @details
#' This function runs a calculation of \deqn{ NDVI = (\lambda_i - \lambda_j)/(\lambda_i + \lambda_j) } using all the possible pairs/combinations of any two bands (i,j)
#' within the full spectrum range thoroughly. A correlation analysis is then performed between the x and all possible NDVIs, and it calculates
#' the correlation coefficients (r) which indicates the predictive performance of each NDVI and its corresponding two-band combination. The
#' output is the wavelength (nm) indicating the best two bands that produce the highest value of r.
#' @seealso \code{\link{cor}}
#' @examples
#' data(NSpec.DF)
#' x <- NSpec.DF$N # nitrogen
#' S <- NSpec.DF$spectra[, seq(1, ncol(NSpec.DF$spectra), 2)] # resampled to 10 nm steps
#' cm.nsr(S, x, cm.plot = TRUE)
#' @import ggplot2 Matrix reshape2 grDevices
#' @export

cm.nsr <- function(spectra, x, w = wavelength(spectra), cm.plot = FALSE){

  # determin the format of spectra
  # if (is(spectra, "Spectra")) w <- wavelength(spectra)
  # if (is(spectra, "data.frame")) w <- wavelength(spectra) # shoudl be numeric
  # if (is(spectra, "matrix")) w <- wavelength(spectra) # shoudl be numeric

  # n <- length(spectra)
  n <- dim(spectra)[2] # Returns the Number of wavebands

  ## (Rj-Ri)/(Rj+Ri)

  R2 <- Matrix::Matrix(0, n, n, sparse = TRUE)  # Zero sparse matrix
  Rj <- spectra

  ones <- matrix(1,1,n)

  for (cI in 1:n){
    Ri <- spectra[,cI]
    Ri <- Ri %*% ones  # to matrix
    # VI formular
    V <- (Rj-Ri)/(Rj+Ri)
    # Squared values (R2) of the Pearson Correlation coefficients
    Rcorr <- (cor(x, V))^2
    # To store the value of R2
    spR <- Matrix::sparseMatrix(i = c(1:n),j = rep(cI,n), x = as.numeric(Rcorr), dims = c(n,n))
    R2 <- R2 + spR
  }
  cm <- as.matrix(R2)
  # str(cm)
  # max(cm, na.rm = TRUE)
  colnames(cm) <- paste(w, "nm")
  if (isTRUE(cm.plot)) plot.cm(cm)
  cm

}


#' Plot the correlation matrix derived from the cm.nsr and cm.sr function
#' @rdname cm.nsr
#' @param cm A square matrix
#' @return
#'   \item{zp1}{Returns a coorrelation matrix plot.}
#' @import ggplot2 reshape2 grDevices RColorBrewer
#' @export
plot.cm <- function(cm){

  #--------------------------------------------------------------------------
  # identify the max R2 and its corresponding bands in a correlation matrix
  #--------------------------------------------------------------------------
  w <- as.numeric(gsub("\\D", "", colnames(cm)))
  R2max <- max(cm, na.rm = TRUE)
  print(paste('The max value of R^2 is', as.character(round(R2max,4))))

  ind_max <- which(cm == R2max, arr.ind = TRUE)
  # ind_max

  bestBands = w[ind_max[1,]]
  print(as.vector(bestBands))

  #----------------------------------
  # plot ndvi
  #----------------------------------

  ZZDF <- reshape2::melt(cm)

  w1_index <- ZZDF$Var1
  w2_index <- ZZDF$Var2
  ZZDF$Var1 <- w[w1_index]
  ZZDF$Var2 <- w[w2_index]

  myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")), space="Lab")

  zp1 <- ggplot(ZZDF, aes(Var1, Var2, fill = value))+
    geom_tile()+
    scale_fill_gradientn(colours = myPalette(100))
  # zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
  # zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
  zp1 <- zp1 + coord_equal()
  zp1 <- zp1 + theme_bw()
  zp1 <- zp1 + xlab("Wavelength i") + ylab("Wavelength j")
  zp1
  print(zp1)
}
