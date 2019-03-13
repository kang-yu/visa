#' @include visa.R
#'
#' An S4 class to represent a `Spectra` object, with four slots.
#'
#' @name Spectra
#' @rdname Spectra-class
#' @export
#' @exportClass Spectra
Spectra <- setClass("Spectra",
                    slots = c(spectra = "matrix",
                              wavelength = "numeric",
                              s.id = "numeric",
                              w.unit = "character"))
setValidity("Spectra",
            function(object){
              w <- length(object)
              if (length(w) != 0L && any(w != w[[1]]))
                return("object width is not constant")
              TRUE
            }
)
setMethod("initialize", "Spectra",
          function(.Object,
                   spectra = matrix(0),
                   wavelength = numeric(0),
                   s.id = numeric(0),
                   w.unit = character(0), ...){
            .Object <- callNextMethod()
            if(ncol(.Object@spectra) != length(.Object@wavelength) && length(.Object@wavelength) >1)
              stop("specified spectra and wavelength of different lengths")
            .Object
          }
)

#'  Construct a Spectra object (S4)
#'
#' This function create a Spectra object
#'
#' @name as.spectra
#' @rdname Spectra-class
#' @param spectra A matrix
#' @param wavelength A numeric vector
#' @param s.id A numeric vector
#' @param w.unit A character
#' @param entry A data.frame
#' @param ... Other parameters
#' @examples
#' \dontrun{
#' s <- as.spectra(matrix(1:100,4), 1:25, 1:4, "nm")
#' s <- as.spectra.entry(data.frame(x = letters[1:4]), matrix(1:100, 4), 1:25, 1:4, "nm")
#' }
#' @export
as.spectra <- function(spectra = matrix(0),
                       wavelength = numeric(0),
                       s.id = numeric(0),
                       w.unit = character(0), ...){
  return(new("Spectra", spectra, wavelength, s.id, w.unit))
}
#' @rdname Spectra-class
#' @param spectra A matrix
#' @param wavelength A numeric vector
#' @param s.id A numeric vector
#' @param w.unit A character
#' @param entry A data.frame
#' @param ... Other parameters
#' @export
as.spectra.entry <- function(entry = data.frame(0),
                             spectra = matrix(0),
                             wavelength = numeric(0),
                             s.id = numeric(0),
                             w.unit = character(0), ...){
  return(new("Spectra", entry, spectra, wavelength, s.id, w.unit, ...))
}


#' The SpectraDataFrame/SpectraEntry S4 class:
#'
#' SpectraDataFrame is an extended the Spectra class, with associated vegetation data (entry)
#' in a \link{data.frame} or \link{list}.
#'
#' @name SpectraDataFrame
#' @rdname SpectraDataFrame-class
#' @aliases SpectraEntry
#'
#' @slot entry A data.frame
#' @slot spectra A matrix
#' @slot wavelength A numeric vector
#' @slot s.id A vector
#' @slot w.unit A character
#' @examples
#' new("Spectra", matrix(1:100,4), 1:25, 1:4, "nm", data.frame(entry=letters[1:4]))
#' @export
#' @exportClass SpectraDataFrame
setClass("SpectraDataFrame",
         contains = "Spectra",
         slots = c(entry="data.frame"),
         validity = function(object){
           w <- length(object)
           if (length(w) != 0L && any(w != w[[1]]))
             return("object width is not constant")
           TRUE
         }
)
setMethod("initialize", "SpectraDataFrame",
          function(.Object,
                   spectra = matrix(0),
                   wavelength = numeric(0),
                   s.id = numeric(0),
                   w.unit = character(0),
                   entry = data.frame(0), ...){
            .Object <- callNextMethod()
            if(nrow(.Object@spectra) != nrow(.Object@entry) && length(.Object@wavelength) >1)
              stop("specified spectra and entry data of different lengths")
            .Object
          }
)


#' Construct a SpectraDataFrame object.
#'
#' This function creates a SpectraDataFrame object, which is equivalent to the use of \link{as.specdf}.
#'
#' @name as.spectra.data.frame
#' @aliases as.specdf
#' @rdname SpectraDataFrame-class
#' @param entry A data.frame
#' @param spectra A matrix
#' @param wavelength A numeric vector
#' @param s.id A numeric vector
#' @param w.unit A character
#' @param ... Other options for similar format of variables
#'
#' @examples
#' sdf <- as.spectra.data.frame(matrix(1:10, 1), 1:10, 1, "nm", data.frame(a = 1, b =2))
#' str(sdf)
#' @export
as.spectra.data.frame <- function(spectra = matrix(0),
                       wavelength = numeric(0),
                       s.id = numeric(0),
                       w.unit = character(0),
                       entry = data.frame(0), ...){
  sls <- new("SpectraDataFrame", spectra, wavelength, s.id, w.unit, entry)
  spec <- sls@spectra
  colnames(spec) <- paste(wavelength, w.unit)
  sdf <- sls@entry
  sdf$spec <- I(spec)
  sdf
}

#' Construct a SpectraMatrix object
#'
#' This function creates a SpectraMatrix object.
#'
#' @name as.spectra.matrix
#' @rdname SpectraMaxtrix-class
#' @param spectra A matrix
#' @param wavelength A numeric vector
#' @param s.id A numeric vector
#' @param w.unit A character
#'
#' @examples
#' smatr <- as.spectra.matrix(matrix(1:10, 1), 1:10, data.frame(a = 1, b =2), "nm")
#' str(smatr)
#' @export
as.spectra.matrix <- function(spectra = matrix(0),
                                  wavelength = numeric(0),
                                  s.id = numeric(0),
                                  w.unit = character(0)){
  sls <- new("SpectraMatrix", spectra, wavelength, s.id, w.unit)
  smat <- sls@spectra
  colnames(smat) <- paste(wavelength, w.unit)
  rownames(smat) <- sls@s.id
  smat
}
