#' @include Spectra-class.R
#'
#' Functions to access slot data of the Class Spectra.
#'
#' Construct generic functions for the Spectra object, spectra.data.frame, and spectra.matrix.
#' A call to {new} returns a newly allocated object from the class identified by the first argument.
#' This call in turn calls the method for the generic function `initialize`.
#'
#' @name spectra
#' @rdname spectra-methods
#' @param object A Spectra object, spectra.data.frame, or spectra.matrix.
#' @param ... Other options
#' @examples
#' \dontrun{
#' # for the Spectra class
#' data(NSpec.DB)
#' spectra_matrix <- spectra(NSpec.DB)
#' # for spectra data.frame
#' data(NSpec.DF)
#' spectra_matrix <- spectra(NSpec.DF)
#' }
#' @export
setGeneric("spectra", function(object, ...) standardGeneric("spectra"))
setMethod("spectra",
          signature(object = "Spectra"),
          function(object, ...){
            mat <- object@spectra
            colnames(mat) <- object@wavelength
            mat
          }
)
setMethod("spectra", signature(object = "data.frame"),
          function(object, ...){
            mat <- object$spectra
            mat
          }
)
setMethod("spectra", signature(object = "matrix"),
          function(object, ...){
            mat <- object
            mat
          }
)
