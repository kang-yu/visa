#' Access the wavelength of Spectra
#'
#' Construct generic functions for the Spectra object, spectra.data.frame, and spectra.matrix.
#'
#' A call to {new} returns a newly allocated object from the class identified by the first argument.
#' This call in turn calls the method for the generic function `initialize`.
#' Construct a Spectra class by using the
#'
#' @name wavelength
#' @describeIn spectra
# @param object A object of Spectra
# @param ... Other options (... T/F with unit)
#' @examples
#' \dontrun{
#' # for Spectra format
#' wavelength(NSpec.DB)
#' # for spectra data.frame format
#' wavelength(NSpec.DF)
#' }
#' @export
# setMethod("as.spectra",
#           signature(spectra = "matrix", wavelength = "numeric"),
#           function(spectra, wavelength, ...){
#             return(as.spectra(spectra, wavelength, ...))
#           }
# )
setGeneric("wavelength", function(object, ...) standardGeneric("wavelength"))
setMethod("wavelength", signature(object = "Spectra"),
          function(object, ...){
            w <- object@wavelength
            w
          }
)
setMethod("wavelength", signature(object = "data.frame"),
          function(object, ...){
            as.numeric(gsub("\\D", "", colnames(object$spectra)))
          }
)
setMethod("wavelength", signature(object = "matrix"),
          function(object, ...){
            as.numeric(gsub("\\D", "", colnames(object)))
          }
)

