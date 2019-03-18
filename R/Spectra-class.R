#' @include visa.R
#'
#' Class 'Spectra' or 'SpectraDatabase'
#'
#' An S4 Class `Spectra`, with five slots of data.
#' It has `SpectraDatabase` as the alias.
#'
#' @name Spectra
#' @aliases SpectraDatabase
#' @rdname Spectra-class
#' @export
#' @exportClass Spectra
Spectra <- setClass("Spectra",
                    slots = c(spectra = "matrix",
                              wavelength = "numeric",
                              w.unit = "character",
                              data = "data.frame"))
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
                   w.unit = character(0),
                   data = data.frame(), ...){
            .Object <- callNextMethod()
            if(length(.Object@data) == 0 && ncol(.Object@spectra) != length(.Object@wavelength)){
              stop("specified spectra and wavelength of different dimensions")
            } else if (length(.Object@data) >= 1 && nrow(.Object@spectra) != nrow(.Object@data)){
              stop("specified spectra and data of different lengths")
            } else
              .Object
          }
)

#' Class 'SpectraDatabase'
#'
#' SpectraDatabase is an extended 'Spectra' class, with associated vegetation data ('data')
#' in a \link{data.frame}.
#'
#' @name SpectraDatabase
#' @rdname SpectraDatabase-class
#' @aliases Spectra
#' @slot spectra A matrix
#' @slot wavelength A numeric vector
#' @slot w.unit A character string
#' @slot data A data.frame of vegetation data corresponding to the spectra
#' @examples
#' new("SpectraDatabase", matrix(1:100,4), 1:25, "nm", data.frame(data=letters[1:4]))
#' @export
setClass("SpectraDatabase",
         contains = "Spectra",
         slots = c(data="data.frame"),
         validity = function(object){
           w <- length(object)
           if (length(w) != 0L && any(w != w[[1]]))
             return("object width is not constant")
           TRUE
         }
)
setMethod("initialize", "SpectraDatabase",
          function(.Object,
                   spectra = matrix(0),
                   wavelength = numeric(0),
                   w.unit = character(0),
                   data = data.frame(0), ...){
            .Object <- callNextMethod()
            if(nrow(.Object@spectra) != nrow(.Object@data) && length(.Object@wavelength) >1)
              stop("specified 'spectra' and 'data' of different lengths")
            .Object
          }
)

#' Create a Spectra or SpectraDatabase
#'
#' This function create a Spectra object.
#'
#' @name as.spectra
#' @aliases as.spectra.database
#' @rdname Spectra-class
#' @param spectra A matrix
#' @param wavelength A numeric vector
#' @param w.unit A character string
#' @param data A data.frame
#' @param ... Other parameters
#' @examples
#' \dontrun{
#' new("Spectra", matrix(1:100,4), 1:25, "nm")
#' s <- as.spectra(matrix(1:100,4), 1:25, "nm")
#' s <- as.spectra.database(matrix(1:100, 4), 1:25, "nm", data.frame(x = letters[1:4]))
#' }
#' @export
as.spectra <- as.spectra.database <- function(spectra = matrix(0),
                                              wavelength = numeric(0),
                                              w.unit = "nm",
                                              data = data.frame(), ...){
  return(new("Spectra", spectra, wavelength, w.unit, data, ...))
}


#' Class 'SpectraMatrix'
#'
#' @name SpectraMatrix
#' @rdname SpectraMaxtrix-class
#' @export
#' @exportClass SpectraMatrix
setClass("SpectraMatrix",
         contains = "Spectra",
         validity = function(object){
           w <- length(object)
           if (length(w) != 0L && any(w != w[[1]]))
             return("object width is not constant")
           TRUE
         }
)
setMethod("initialize", "SpectraMatrix",
          function(.Object,
                   spectra = matrix(0),
                   wavelength = numeric(0),
                   w.unit = character(0), ...){
            .Object <- callNextMethod()
            if(ncol(.Object@spectra) != length(.Object@wavelength))
              stop("specified spectra and wavelength are of different lengths")
            .Object
          }
)
#' Create a SpectraMatrix
#'
#' This function creates a SpectraMatrix object.
#'
#' @name as.spectra.matrix
#' @rdname SpectraMaxtrix-class
#' @param spectra A matrix
#' @param wavelength A numeric vector
#' @param w.unit A character string
#' @examples
#' smatrix <- as.spectra.matrix(matrix(1:10, 1), 1:10, "nm")
#' str(smatrix)
#' @export
as.spectra.matrix <- function(spectra = matrix(0),
                              wavelength = numeric(0),
                              w.unit = character(0)){
  sls <- new("SpectraMatrix", spectra, wavelength, w.unit)
  smat <- sls@spectra
  colnames(smat) <- paste(wavelength, w.unit)
  # rownames(smat) <- sls@s.id
  smat
}

#' Class 'SpectraDataFrame'
#'
#' SpectraDataFrame is an extended 'Spectra' class, with associated vegetation data ('data')
#' in a \link{data.frame}.
#'
#' @name SpectraDataFrame
#' @rdname SpectraDataFrame-class
#' @aliases spectra.data.frame
#' @slot spectra A matrix
#' @slot wavelength A numeric vector
#' @slot w.unit A character string
#' @slot data A data.frame of vegetation data corresponding to the spectra
#' @examples
#' new("SpectraDataFrame", matrix(1:100,4), 1:25, "nm", data.frame(data=letters[1:4]))
#' @export
#' @exportClass SpectraDataFrame
setClass("SpectraDataFrame",
         contains = "Spectra",
         slots = c(data="data.frame"),
         validity = function(object){
           w <- length(object)
           if (length(w) != 0L && any(w != w[[1]]))
             return("object width is not constant")
           TRUE
         }
)
setMethod("initialize", "SpectraDataFrame",
          function(.Object,
                   spectra = matrix,
                   wavelength = numeric,
                   w.unit = character,
                   data = data.frame, ...){
            .Object <- callNextMethod()
            if(nrow(.Object@spectra) != nrow(.Object@data) && length(.Object@wavelength) >1)
              stop("specified 'spectra' and 'data' of different lengths")
            .Object
          }
)
#' Create a SpectraDataFrame
#'
#' This function creates a SpectraDataFrame object, which is equivalent to the use of \link{as.specdf}.
#'
#' @name as.spectra.data.frame
#' @aliases as.specdf
#' @rdname SpectraDataFrame-class
#' @param data A data.frame
#' @param spectra A matrix
#' @param wavelength A numeric vector
#' @param w.unit A character string
#' @param ... Other options for similar format of variables
#' @examples
#' sdf <- as.spectra.data.frame(matrix(1:10, 1), 1:10, "nm", data.frame(a = 1, b =2))
#' str(sdf)
#' @export
as.spectra.data.frame <- function(spectra = matrix(0),
                                  wavelength = numeric(0),
                                  w.unit = character(0),
                                  data = data.frame(0), ...){
  sls <- new("SpectraDataFrame", spectra, wavelength, w.unit, data)
  spec <- sls@spectra
  colnames(spec) <- paste(wavelength, w.unit)
  sdf <- sls@data
  sdf$spec <- I(spec)
  sdf
}


