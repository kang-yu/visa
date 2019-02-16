#' Spectral data frame.
#'
#' A dataset containing the plant Nitrogen content and spectra. Spectra matrix is stored as a variable of data frame.
#'
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
#' data(NSpec_List)
#' str(NSpec_List)
#' @source \url{visa}
"NSpec_List"
