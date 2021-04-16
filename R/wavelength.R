<<<<<<< HEAD
#' Access the wavelength of Spectra
#'
#' Construct generic functions for the Spectra object, spectra.data.frame, and spectra.matrix.
#'
#' A call to {new} returns a newly allocated object from the class identified by the first argument.
#' This call in turn calls the method for the generic function `initialize`.
#' Construct a Spectra class by using the
#'
#' @docType methods
#' @name wavelength
#' @rdname wavelength-methods
#' @aliases waveband
#' @param object A object of Spectra
#' @param ... Other options (... T/F with unit)
#' @examples
#' library(visa)
#' # For S4 class Spectra
#' wavelength(NSpec.DB)
#' # For spectra data.frame format
#' wavelength(NSpec.DF)
#'
#' @export wavelength
# setMethod("as.spectra",
#           signature(spectra = "matrix", wavelength = "numeric"),
#           function(spectra, wavelength, ...){
#             return(as.spectra(spectra, wavelength, ...))
#           }
# )
setGeneric("wavelength", function(object, ...) standardGeneric("wavelength"))

#' @rdname wavelength-methods
#' @aliases wavelength,Spectra,ANY-method
setMethod("wavelength", signature(object = "Spectra"),
          function(object, ...){
            w <- object@wavelength
            w
          }
)

#' @rdname wavelength-methods
#' @aliases wavelength,data.frame,ANY-method
setMethod("wavelength", signature(object = "data.frame"),
          function(object, ...){
            as.numeric(gsub("\\D", "", colnames(object$spectra)))
          }
)

#' @rdname wavelength-methods
#' @aliases wavelength,matrix,ANY-method
setMethod("wavelength", signature(object = "matrix"),
          function(object, ...){
            as.numeric(gsub("\\D", "", colnames(object)))
          }
)

=======
#' Access the wavelength of Spectra
#'
#' Construct generic functions for the Spectra object, spectra.data.frame, and spectra.matrix.
#'
#' A call to {new} returns a newly allocated object from the class identified by the first argument.
#' This call in turn calls the method for the generic function `initialize`.
#' Construct a Spectra class by using the
#'
#' @docType methods
#' @name wavelength
#' @rdname wavelength-methods
#' @aliases waveband
#' @param object A object of Spectra
#' @param ... Other options (... T/F with unit)
#' @examples
#' library(visa)
#' # For S4 class Spectra
#' wavelength(NSpec.DB)
#' # For spectra data.frame format
#' wavelength(NSpec.DF)
#'
#' @export wavelength
# setMethod("as.spectra",
#           signature(spectra = "matrix", wavelength = "numeric"),
#           function(spectra, wavelength, ...){
#             return(as.spectra(spectra, wavelength, ...))
#           }
# )
setGeneric("wavelength", function(object, ...) standardGeneric("wavelength"))

#' @rdname wavelength-methods
#' @aliases wavelength,Spectra,ANY-method
setMethod("wavelength", signature(object = "Spectra"),
          function(object, ...){
            w <- object@wavelength
            w
          }
)

#' @rdname wavelength-methods
#' @aliases wavelength,data.frame,ANY-method
setMethod("wavelength", signature(object = "data.frame"),
          function(object, ...){
            as.numeric(gsub("\\D", "", colnames(object$spectra)))
          }
)

#' @rdname wavelength-methods
#' @aliases wavelength,matrix,ANY-method
setMethod("wavelength", signature(object = "matrix"),
          function(object, ...){
            as.numeric(gsub("\\D", "", colnames(object)))
          }
)

>>>>>>> fdcc887ca45316c73fbe6ef70768b77deffbc575
