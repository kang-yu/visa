#' @include visa.R
#'
#' An S4 class to represent a Spec object.
#'
#' @export
#' @exportClass Spectra

setClass("Spectra",
         slots = c(spectra = "matrix",
                   wavelength = "numeric",
                   s.id = "vector",
                   w.unit = "character"))

Spectra <- function(spectra, wavelength , s.id , w.unit)
  new("Spectra", spectra=spectra, wavelength=wavelength, s.id=s.id, w.unit=w.unit)


setValidity("Spectra",
            function(object){
              w <- length(object)
              if (length(w) != 0L && any(w != w[[1]]))
                return("object width is not constant")
              TRUE
            }
)


#' Class Spectra extended with associated vegetation data
#' @rdname Spectra
#'
#' @slot entry A data.frame
#' @slot spectra A matrix
#' @slot wavelength A numeric vector
#' @slot s.id A vector
#' @slot w.unit A character
#' @exportClass SpectraDataFrame

setClass("SpectraDataFrame", contains = "Spectra",
  slots = c(entry="data.frame"),
  validity = function(object){
    w <- width(object)
    if (length(w) != 0L && any(w != w[[1]]))
      return("object width is not constant")
    TRUE
  }
)
