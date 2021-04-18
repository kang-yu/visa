#' Example data in the SpectraDataFrame format
#'
#' A dataset containing the plant Nitrogen content and spectra.
#' The Spectra matrix is stored as a variable (in a column) of a data.frame.
#'
#' @aliases Data-SpectraDataFrame
#' @format A data frame with 19 rows and 2 variables:
#'
#' \describe{
#'   \item{N}{Plant nitrogen content}
#'   \item{spectra}{A variable of Matrix of plant spectra}
#'   ...
#' }
#' @examples
#' library(visa)
#' data(NSpec.DF)
#' str(NSpec.DF)
#' @seealso \link{data.frame} and \code{\link{NSpec.DB}}
#'
"NSpec.DF"

