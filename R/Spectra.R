#' @include Spectra-class.R
#'
#' Functions to access slot data of the Spectra object.
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
#' data(NSpec_List)
#' spectra_matrix <- spectra(NSpec_List)
#' # for spectra.data.frame
#' data(NSpec_DF)
#' spectra_matrix <- spectra(NSpec_DF)
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


#' Create a SpectraDataFrame
#'
#' This function creates a SpectraDataFrame object, which is equivalent to the use of \link{as.specdf}.
#'
#' @name as.spectra.data.frame
#' @aliases as.specdf
#' @rdname spectra-methods
#' @param data A data.frame
#' @param spectra A matrix
#' @param wavelength A numeric vector
#' @param s.id A vector
#' @param w.unit A character string
#' @param ... Other options for similar format of variables
#'
#' @examples
#' sdf <- as.spectra.data.frame(matrix(1:10, 1), 1:10, 1, "nm", data.frame(a = 1, b =2))
#' str(sdf)
#' @export
as.spectra.data.frame <- function(spectra = matrix(0),
                                  wavelength = numeric(0),
                                  s.id = vector(),
                                  w.unit = character(0),
                                  data = data.frame(0), ...){
  sls <- new("SpectraDataFrame", spectra, wavelength, s.id, w.unit, data)
  spec <- sls@spectra
  colnames(spec) <- paste(wavelength, w.unit)
  sdf <- sls@data
  sdf$spec <- I(spec)
  sdf
}
