#' @include Spectra-class.R

#' Construct generic functions for the Spectra object.
#'
#' A call to {new} returns a newly allocated object from the class identified by the first argument.
#' This call in turn calls the method for the generic function `initialize`.
#' Construct a Spectra class by using the
#'
#' @name wavelength
#' @rdname spectra-methods
#' @aliases get.wavelength
#' @param object A object of Spectra
#' @param ... Other options (... T/F with unit)
#' @examples
#' \dontrun{
#' wavelength(NSpec_List)
#' }
#' @export
# setMethod("as.spectra",
#           signature(spectra = "matrix", wavelength = "numeric"),
#           function(spectra, wavelength, ...){
#             return(as.spectra(spectra, wavelength, ...))
#           }
# )
setGeneric("wavelength", function(object, ...) standardGeneric("wavelength"))
setMethod("wavelength",
          signature(object = "Spectra"),
          function(object, ...){
            w <- object@wavelength
            w
          }
)



#' Functions to access slot data of the Spectra object.
#'
#' @name spectra
#' @aliases get.spectra
#' @rdname spectra-methods
#'
#' @param object A Spectra object
#' @examples
#' data(NSpec_List)
#' spectra_matrix <- spectra(NSpec_List)
#' @export
#' @exportMethod spectra
setGeneric("spectra", function(object, ...) standardGeneric("spectra"))
setMethod("spectra",
          signature(object = "Spectra"),
          function(object, ...){
            mat <- object@spectra
            colnames(mat) <- object@wavelength
            mat
          }
)
