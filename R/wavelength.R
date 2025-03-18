#' Retrieve Wavelength Information from Spectra Objects
#'
#' This function extracts the wavelength information from various representations of spectra.
#' It supports the S4 class \code{Spectra}, as well as data.frame and matrix representations.
#'
#' For an object of class \code{Spectra}, the method returns the value stored in the \code{wavelength}
#' slot. For a data.frame or matrix, it extracts numeric values from the column names (by removing
#' non-digit characters) of the spectra data.
#'
#' @docType methods
#' @name wavelength
#' @rdname wavelength
#' @aliases wavlen
#'
#' @param object An object containing spectra data. This can be an S4 object of class \code{Spectra},
#'   a \code{data.frame}, or a \code{matrix}.
#' @param ... Additional arguments for future extensions (currently not used).
#'
#' @return A numeric vector representing the wavelength information extracted from the object.
#'
#' @examples
#' \dontrun{
#'   library(visa)
#'
#'   # For an S4 Spectra object
#'   wavelengths <- wavelength(NSpec.Lib)
#'
#'   # For spectra stored in a data.frame
#'   wavelengths <- wavelength(NSpec.DF)
#'
#'   # For spectra stored in a matrix
#'   wavelengths <- wavelength(spectra_matrix)
#' }
#'
#' @export
setGeneric("wavelength", function(object, ...) standardGeneric("wavelength"))

#' @rdname wavelength
#' @aliases wavelength,Spectra,ANY-method
setMethod("wavelength", signature(object = "Spectra"),
          function(object, ...) {
            object@wavelength
          }
)

#' @rdname wavelength
#' @aliases wavelength,data.frame,ANY-method
setMethod("wavelength", signature(object = "data.frame"),
          function(object, ...) {
            as.numeric(gsub("\\D", "", colnames(object$spectra)))
          }
)

#' @rdname wavelength
#' @aliases wavelength,matrix,ANY-method
setMethod("wavelength", signature(object = "matrix"),
          function(object, ...) {
            as.numeric(gsub("\\D", "", colnames(object)))
          }
)
