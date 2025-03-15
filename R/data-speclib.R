#' Example data in the Spectra/SpectraLibrary format.
#'
#' A S4 data structure containing the plant spectra and nitorgen (N) content. Spectra is organized as a matrix and is stored as a slot,
#' named 'spectra'. The corresponding N content is stored in the slot 'data', which is a data.frame used for storing supporting data and
#' plant/vegetation traits, such as here the plant N content.
#'
#' @aliases Data-SpectraLibrary, Data-speclib, Data-Spectra
#' @format A Spectra object with 19 rows and 4 slots (spectra, wavelength, w.unit, data).
#'
#' \describe{
#'   \item{spectra}{A matrix of plant spectral data}
#'   \item{wavelength}{A vector of wavelength for the 'spectra' data}
#'   \item{w.unit}{A character string of wavelength unit (default "nm")}
#'   \item{data}{A data.frame of vegetation traits, here plant nitrogen content}
#'    \code{...{currently not used}}
#' }
#' @examples
#' library(visa)
#' data(NSpec.Lib)
#' str(NSpec.Lib)
#'
"NSpec.Lib"
