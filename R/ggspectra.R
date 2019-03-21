#' Create a new ggplot plot from spectra data
#'
#' \code{ggplot()} initializes a ggplot object. It can be used to
#' declare the input spectral object for a graphic and to optionally specify the
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
#' There are three common ways to invoke \code{ggplot}:
#' \itemize{
#'    \item \code{ggplot(ts, aes(x, y, <other aesthetics>))}
#'    \item \code{ggplot(ts)}
#'   }
#' The first method is recommended if all layers use the same
#' data and the same set of aesthetics, although this method
#' can also be used to add a layer using data from another
#' data frame. See the first example below. The second
#' method specifies the default spectrum object to use for the plot, and the
#' units to be used for y in the plot,
#' but no aesthetics are defined up front. This is useful when
#' one data frame is used predominantly as layers are added,
#' but the aesthetics may vary from one layer to another. The
#' third method specifies the default spectrum object to use for the plot,
#' but no aesthetics are defined up front. This is useful when
#' one spectrum is used predominantly as layers are added,
#' but the aesthetics may vary from one layer to another.
#'
#' @param data Default spectra database to use for plot. If not a spectra database, the
#'   methods used will be those defined in package \code{ggplot2}. See \code{\link[ggplot2]{ggplot}}.
#'   If not specified,
#'   must be suppled in each layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot.
#'   If not specified, in the case of spectra objects, a default mapping will
#'   be used.
#' @param wl numeric The wavelength vector.
#' @param w.unit character The wavelength unit fo the spectra.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @param environment If an variable defined in the aesthetic mapping is not
#'   found in the data, ggplot will look for it in this environment. It defaults
#'   to using the environment in which \code{ggplot()} is called.
#'
#' @seealso \code{?ggpmisc::ggplot()}
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot.spectra(NSpec.DF) + geom_line()
#' }
#' @note Current implementation does not merge default mapping with user
#' supplied mapping. If user supplies a mapping, it is used as is.
#' To add to the default mapping, aes() can be used by itself to compose
#' the ggplot.
#'
#' @import reshape2 ggplot2
#' @name ggplot
#' @export
ggplot.spectra <- function(data, mapping = NULL, ...,
           wl = NULL,
           w.unit = "nm",
           environment = parent.frame()) {
    spec <- spectra(data)
    # wl <- wavelength(data)
    specdf <- melt(spec)
    specdf$Var2 <- as.numeric(gsub("\\D", "", specdf$Var2))
    if (is.null(mapping)) {
      mapping <- aes_string("Var2", "value")
    }
    ggplot2::ggplot(data = specdf,
                    mapping =  mapping,
                    ... = ...,
                    environment = environment)
  }


