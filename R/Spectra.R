#' @include Spectra-class.R

#' Construct generic functions for the Spectra object.
#'
#' A call to {new} returns a newly allocated object from the class identified by the first argument.
#' This call in turn calls the method for the generic function `initialize`.
#' Construct a Spectra class by using the
#'
#' @name wavelength
#' @rdname spectra-methods
#'
#' @param object A object of Spectra
#' @param ... Other options (... T/F with unit)
#' @examples
#' \dontrun{
#' # for Spectra format
#' wavelength(NSpec.LS)
#' # for Spectra data.frame format
#' wavelength(NSpec.DS)
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




#' Functions to access slot data of the Spectra object.
#'
#' @name spectra
#' @rdname spectra-methods
#'
#' @param object A Spectra object
#' @param ... Other options
#' @examples
#' # for the Spectra class
#' data(NSpec_List)
#' spectra_matrix <- spectra(NSpec_List)
#' # for spectra.data.frame
#' data(NSpec_DF)
#' spectra_matrix <- spectra(NSpec_DF)
#'
#' @export
#' @exportMethod spectra

# setOldClass("data.frame")
# spectra <- function(object){
#   if (is(object, "data.frame")) mat <- object$spectra
#   if (is(object, "Spectra")) mat <- object@spectra
#   mat
# }
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

# spectra.data.frame <- function(object, spectra.name = "spectra", ...) object[spectra.name]




