#' @include visa.R
#'
#' Class 'Spectra'
#'
#' An S4 Class \code{Spectra} with four slots: \code{spectra}, \code{wavelength}, \code{w.unit}, and \code{data}.
#'
#' @name Spectra-class
#' @rdname Spectra-class
#' @aliases Spectra, Spectra-class
#' @docType class
#' @slot spectra A matrix.
#' @slot wavelength A numeric vector.
#' @slot w.unit A character string.
#' @slot data A data.frame.
#' @importFrom methods new
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
            .Object <- methods::callNextMethod()
            if(length(.Object@data) == 0 && ncol(.Object@spectra) != length(.Object@wavelength)){
              stop("specified spectra and wavelength of different dimensions")
            } else if (length(.Object@data) >= 1 && nrow(.Object@spectra) != nrow(.Object@data)){
              stop("specified spectra and data of different lengths")
            } else
              .Object
          }
)

#' Class 'Spectra.Dataframe'
#'
#' \code{Spectra.Dataframe} is an extended \code{Spectra} class with associated vegetation data in a \code{data.frame}.
#'
#' @name Spectra.Dataframe-class
#' @rdname Spectra.Dataframe-class
#' @aliases Spectra.Dataframe-class, spectra.df
#' @docType class
#' @slot spectra A matrix.
#' @slot wavelength A numeric vector.
#' @slot w.unit A character string.
#' @slot data A data.frame of vegetation data corresponding to the spectra.
#' @exportClass Spectra.Dataframe
setClass("Spectra.Dataframe",
         contains = "Spectra",
         slots = c(data = "data.frame"),
         validity = function(object){
           w <- length(object)
           if (length(w) != 0L && any(w != w[[1]]))
             return("object width is not constant")
           TRUE
         }
)
setMethod("initialize", "Spectra.Dataframe",
          function(.Object,
                   spectra = matrix(0),
                   wavelength = numeric(0),
                   w.unit = character(0),
                   data = data.frame(0), ...){
            .Object <- methods::callNextMethod()
            if(nrow(.Object@spectra) != nrow(.Object@data) && length(.Object@wavelength) > 1)
              stop("specified spectra and data of different lengths")
            .Object
          }
)

#' Create a Spectra Object
#'
#' The constructor \code{as.spectra} creates a \code{Spectra} object.
#'
#' @name as.spectra
#' @rdname Spectra-class
#' @param spectra A matrix.
#' @param wavelength A numeric vector.
#' @param w.unit A character string.
#' @param data A data.frame.
#' @param ... Other parameters.
#' @examples
#' s <- as.spectra(matrix(1:100, 4), 1:25, "nm", data.frame(x = letters[1:4]))
#' str(s)
#' @export
as.spectra <- function(spectra = matrix(0),
                       wavelength = numeric(0),
                       w.unit = "nm",
                       data = data.frame(), ...){
  return(methods::new("Spectra", spectra, wavelength, w.unit, data, ...))
}

#' Create a Spectra Dataframe Object
#'
#' The constructor \code{as.spectra.dataframe} creates a \code{Spectra.Dataframe} object.
#'
#' @rdname Spectra-class
#' @examples
#' s <- as.spectra.dataframe(matrix(1:100, 4), 1:25, "nm", data.frame(x = letters[1:4]))
#' str(s)
#' @export
as.spectra.dataframe <- function(spectra = matrix(0),
                                 wavelength = numeric(0),
                                 w.unit = "nm",
                                 data = data.frame(), ...){
  return(methods::new("Spectra", spectra, wavelength, w.unit, data, ...))
}

#' Class 'Spectra.Matrix'
#'
#' \code{Spectra.Matrix} is an extended \code{Spectra} class.
#'
#' @name Spectra.Matrix-class
#' @rdname Spectra.Matrix-class
#' @aliases Spectra.Matrix-class, spectra.matrix
#' @docType class
#' @exportClass Spectra.Matrix
setClass("Spectra.Matrix",
         contains = "Spectra",
         validity = function(object){
           w <- length(object)
           if (length(w) != 0L && any(w != w[[1]]))
             return("object width is not constant")
           TRUE
         }
)
setMethod("initialize", "Spectra.Matrix",
          function(.Object,
                   spectra = matrix(0),
                   wavelength = numeric(0),
                   w.unit = character(0), ...){
            .Object <- methods::callNextMethod()
            if(ncol(.Object@spectra) != length(.Object@wavelength))
              stop("specified spectra and wavelength are of different lengths")
            .Object
          }
)

#' Create a Spectra Matrix Object
#'
#' The constructor \code{as.spectra.matrix} creates a \code{Spectra.Matrix} object.
#'
#' @name as.spectra.matrix
#' @rdname Spectra.Matrix-class
#' @param spectra A matrix.
#' @param wavelength A numeric vector.
#' @param w.unit A character string.
#' @return A matrix with column names set to the wavelengths.
#' @examples
#' smatrix <- as.spectra.matrix(matrix(1:10, 1), 1:10, "nm")
#' str(smatrix)
#' @export
as.spectra.matrix <- function(spectra = matrix(0),
                              wavelength = numeric(0),
                              w.unit = character(0)){
  # Note: Updated to use "Spectra.Matrix" instead of "SpectraMatrix"
  sls <- methods::new("Spectra.Matrix", spectra, wavelength, w.unit)
  smat <- sls@spectra
  colnames(smat) <- paste(wavelength, w.unit)
  smat
}

#' Class 'Spectra.Dataframe'
#'
#' \code{Spectra.Dataframe} is an extended \code{Spectra} class with associated vegetation data in a \code{data.frame}.
#'
#' @name Spectra.Dataframe-class
#' @rdname Spectra.Dataframe-class
#' @aliases Spectra.Dataframe, spectra.data.frame
#' @docType class
#' @slot spectra A matrix.
#' @slot wavelength A numeric vector.
#' @slot w.unit A character string.
#' @slot data A data.frame of vegetation data corresponding to the spectra.
#' @exportClass Spectra.Dataframe
setClass("Spectra.Dataframe",
         contains = "Spectra",
         slots = c(data = "data.frame"),
         validity = function(object){
           w <- length(object)
           if (length(w) != 0L && any(w != w[[1]]))
             return("object width is not constant")
           TRUE
         }
)
setMethod("initialize", "Spectra.Dataframe",
          function(.Object,
                   spectra = matrix,
                   wavelength = numeric,
                   w.unit = character,
                   data = data.frame, ...){
            .Object <- methods::callNextMethod()
            if(nrow(.Object@spectra) != nrow(.Object@data) && length(.Object@wavelength) > 1)
              stop("specified 'spectra' and 'data' of different lengths")
            .Object
          }
)

#' Create a Spectra DataFrame Object
#'
#' The constructor \code{as.spectra.data.frame} creates a \code{SpectraDataFrame} object.
#'
#' @name as.spectra.data.frame
#' @rdname Spectra.Dataframe-class
#' @aliases as.specdf
#' @param spectra A matrix.
#' @param wavelength A numeric vector.
#' @param w.unit A character string.
#' @param data A data.frame.
#' @param ... Other options.
#' @return A \code{Spectra.Dataframe} with the spectra embedded in the data.frame.
#' @examples
#' sdf <- as.spectra.data.frame(matrix(1:10, 1), 1:10, "nm", data.frame(a = 1, b = 2))
#' str(sdf)
#' @export
as.spectra.data.frame <- function(spectra = matrix(0),
                                  wavelength = numeric(0),
                                  w.unit = character(0),
                                  data = data.frame(0), ...){
  sls <- methods::new("Spectra.Dataframe", spectra, wavelength, w.unit, data)
  spectra <- sls@spectra
  colnames(spectra) <- paste(wavelength, w.unit)
  sdf <- sls@data
  sdf$spectra <- spectra
  sdf
}
