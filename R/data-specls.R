#' Example data in the Spectra class format.
#'
#' A dataset containing the plant Nitrogen content and spectra. Spectra matrix is stored as a variable of data frame.
#'
#' @aliases Data-Spectra
#' @format A Spectra object with 19 rows and 4 slots (spectra, wavelength, s.id, w.unit).
#'
#' \describe{
#'   \item{N}{Plant nitrogen content}
#'   \item{spectra}{A variable of Matrix of plant spectra}
#'   \item{s.id}{A vector of spectra IDs}
#'   \item{w.unit}{A character string of wavelength unit ("nm")}
#'   ...
#' }
#' @examples
#' data(NSpec.DB)
#' str(NSpec.DB)
#' @source \url{visa}
"NSpec.DB"
