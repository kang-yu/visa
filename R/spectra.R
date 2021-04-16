#' Access the spectra data of 'SpectraDatabase'.
#'
#' Functions to access slot data of the Class Spectra.
#'
#' Construct generic functions for the Spectra object, spectra.data.frame,
#' and spectra.matrix.
#'
#' @include Spectra-class.R
#' @docType methods
#' @rdname spectra-methods
#' @name spectra
#' @aliases spectra
#' @param object A Spectra object, spectra.data.frame, or spectra.matrix.
#' @param ... Other options.
#' @examples
#' # For the S4 class 'Spectra'
#' library(visa)
#' data(NSpec.DB)
#' spectra_matrix <- spectra(NSpec.DB)
#' # For the spectra data.frame
#' data(NSpec.DF)
#' spectra_matrix <- spectra(NSpec.DF)
#'
#' @export spectra
setGeneric("spectra", function(object, ...) standardGeneric("spectra"))

#' @rdname spectra-methods
#' @aliases spectra,Spectra,ANY-method
setMethod("spectra",
          signature(object = "Spectra"),
          function(object, ...){
            mat <- object@spectra
            colnames(mat) <- object@wavelength
            mat
          }
)

#' @rdname spectra-methods
#' @aliases spectra,data.frame,ANY-method
setMethod("spectra", signature(object = "data.frame"),
          function(object, ...){
            mat <- object$spectra
            mat
          }
)

#' @rdname spectra-methods
#' @aliases spectra,matrix,ANY-method
setMethod("spectra", signature(object = "matrix"),
          function(object, ...){
            mat <- object
            mat
          }
)

