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
#' @name ggplot.spectra
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



