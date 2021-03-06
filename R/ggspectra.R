#' Create a new ggplot plot with a geom_line() layer from spectra data
#'
#' \code{ggplot()} initializes a ggplot object. It can be used to
#' declare the input spectra object for a graphic and to optionally specify the
#' set of plot aesthetics intended to be common throughout all
#' subsequent layers unless specifically overridden.
#'
#' \code{ggplot()} is typically used to construct a plot
#' incrementally, using the + operator to add layers to the
#' existing ggplot object. This is advantageous in that the
#' code is explicit about which layers are added and the order
#' in which they are added. For complex graphics with multiple
#' layers, initialization with \code{ggplot} is recommended.
#'
#'
#' @param data Default spectra database to use for plot. If not a spectra database, the
#'   methods used will be those defined in package \code{ggplot2}. See \code{\link[ggplot2]{ggplot}}.
#'   If not specified, must be supplied in each layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot.
#'   If not specified, in the case of spectra objects, a default mapping will
#'   be used.
#' @param wl numeric The wavelength vector.
#' @param w.unit character The wavelength unit of the spectra.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @param environment If an variable defined in the aesthetic mapping is not
#'   found in the data, ggplot will look for it in this environment. It defaults
#'   to using the environment in which \code{ggplot()} is called.
#'
#' @seealso \code{?ggpmisc::ggplot()}
#' @examples
#' library(visa)
#' library(ggplot2)
#' ggplot.spectra(NSpec.DF)
#'
#' @note Current implementation does not merge default mapping with user
#' supplied mapping. If user supplies a mapping, it is used as is.
#' To add to the default mapping, aes() can be used by itself to compose
#' the ggplot.
#'
#' @import reshape2 ggplot2
#' @name ggplot
#' @export ggplot.spectra
ggplot.spectra <- function(data, mapping = NULL, ...,
                           wl = NULL,
                           w.unit = "nm",
                           environment = parent.frame()) {
  spec <- spectra(data)
  # wl <- wavelength(data)
  specdf <- melt(spec)
  # "Var1" is the rownames
  specdf$Var1 <- as.factor(specdf$Var1)
  specdf$Var2 <- as.numeric(gsub("\\D", "", specdf$Var2))
  names(specdf)[1:2] <- c("spectrum_id", "band")
  if (is.null(mapping)) {
    mapping <- aes_string(x = "band", y = "value",
                          group = "spectrum_id",
                          color = "spectrum_id")
  }
  ggplot2::ggplot(data = specdf,
                  mapping = mapping,
                  ... = ...,
                  environment = environment) +
    geom_line(show.legend = FALSE)
}


#' Create a new ggplot plot from the correlation matrix derived from the
#' cm.nsr and cm.sr output.
#'
#' @rdname ggplot
#'
#' @param show.stat A logic value. whether show the best R^2 and bands.
#' @return
#'   \item{cm_plot}{Returns a ggplot object of correlation-matrix.}
#'
#' @examples
#' library(visa)
#' data(NSpec.DF)
#' x <- NSpec.DF$N # nitrogen
#' S <- NSpec.DF$spectra[, seq(1, ncol(NSpec.DF$spectra), 5)] # resampled to 10 nm steps
#' cm <- cm.sr(S, x, cm.plot = FALSE)
#' ggplot.cm(cm)
#' @import ggplot2 reshape2 grDevices
#' @export ggplot.cm

ggplot.cm <- function(data, mapping = NULL, ...,
                      show.stat = TRUE,
                      environment = parent.frame()){

  # Identify the max R2 and its corresponding bands in a correlation matrix
  w <- as.numeric(gsub("\\D", "", colnames(data)))
  R2max <- max(data, na.rm = TRUE)
  if (show.stat) print(paste('The max value of R^2 is', as.character(round(R2max,4))))
  ind_max <- which(data == R2max, arr.ind = TRUE)
  # ind_max
  bestBands = w[ind_max[1,]]
  if (show.stat) print(paste(c("i", "j"), as.vector(bestBands), sep = "_"))

  # plot correlation matrix

  cmDF <- reshape2::melt(data)
  w1_index <- cmDF$Var1
  w2_index <- cmDF$Var2
  cmDF$Var1 <- w[w1_index]
  cmDF$Var2 <- w[w2_index]

  myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")), space="Lab")

  if (is.null(mapping)) {
    mapping <- aes_string("Var1", "Var2", fill = "value")
  }
  cmp <- ggplot2::ggplot(cmDF, mapping = mapping,
                         ... = ...,
                         environment = environment)+
    geom_tile()+
    scale_fill_gradientn(colours = myPalette(100))+
    coord_equal()+
    theme_bw()
  cm_plot <- cmp + xlab("Wavelength i") + ylab("Wavelength j")
  cm_plot

  # cm_plot <- cm_plot + scale_x_discrete(expand = c(0, 0))+
  #   scale_y_discrete(expand = c(0, 0))
  # print(cm_plot)
}


