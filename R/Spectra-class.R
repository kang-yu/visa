<<<<<<< HEAD
#' @include visa.R
#'
#' Class 'Spectra'/'SpectraDatabase'
#'
#' An S4 Class `Spectra`, with five slots of data.
#' It has `SpectraDatabase` as the alias.
#'
#' @name Spectra-class
#' @rdname Spectra-class
#' @aliases Spectra,Spectra-class
#' @docType class
#' @slot spectra A matrix
#' @slot wavelength A numeric vector
#' @slot w.unit A character string
#' @slot data A data.frame
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
#' Class 'SpectraDatabase'
#'
#' SpectraDatabase is an extended 'Spectra' class, with associated vegetation data ('data')
#' in a \link{data.frame}.
#'
#' @name SpectraDatabase-class
#' @rdname SpectraDatabase-class
#' @aliases SpectraDatabase-class,spectra.database
#' @docType class
#' @slot spectra A matrix
#' @slot wavelength A numeric vector
#' @slot w.unit A character string
#' @slot data A data.frame of vegetation data corresponding to the spectra
#' @export
#' @exportClass SpectraDatabase
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
            .Object <- methods::callNextMethod()
            if(nrow(.Object@spectra) != nrow(.Object@data) && length(.Object@wavelength) >1)
              stop("specified 'spectra' and 'data' of different lengths")
            .Object
          }
)

#' Create a Spectra or SpectraDatabase
#'
#' Constructor \code{as.spectra} creates a Spectra object.
#'
#' @name as.spectra
#' @rdname Spectra-class
#' @param spectra A matrix
#' @param wavelength A numeric vector
#' @param w.unit A character string
#' @param data A data.frame
#' @param ... Other parameters
#' @examples
#' s <- as.spectra(matrix(1:100, 4), 1:25, "nm", data.frame(x = letters[1:4]))
#' str(s)
#'
#' @export
as.spectra <- function(spectra = matrix(0),
                       wavelength = numeric(0),
                       w.unit = "nm",
                       data = data.frame(), ...){
  return(methods::new("Spectra", spectra, wavelength, w.unit, data, ...))
}
#' Create a Spectra or SpectraDatabase
#'
#' Constructor \code{as.spectra.database} creates a SpectraDatabase object.
#'
#' @rdname Spectra-class
#' @examples
#' s <- as.spectra.database(matrix(1:100, 4), 1:25, "nm", data.frame(x = letters[1:4]))
#' str(s)
#' @export
as.spectra.database <- function(spectra = matrix(0),
                                wavelength = numeric(0),
                                w.unit = "nm",
                                data = data.frame(), ...){
  return(methods::new("Spectra", spectra, wavelength, w.unit, data, ...))
}


#' Class 'SpectraMatrix'
#'
#' SpectraMatrix is a extended 'Spectra' class.
#'
#' @name SpectraMatrix-class
#' @rdname SpectraMaxtrix-class
#' @aliases SpectraMaxtrix-class,spectra.maxtrix
#' @docType class
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
            .Object <- methods::callNextMethod()
            if(ncol(.Object@spectra) != length(.Object@wavelength))
              stop("specified spectra and wavelength are of different lengths")
            .Object
          }
)
#' Create a SpectraMatrix
#'
#' Constructor \code{as.spectra.matrix} creates a SpectraMatrix object.
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
  sls <- methods::new("SpectraMatrix", spectra, wavelength, w.unit)
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
#' @name SpectraDataFrame-class
#' @rdname SpectraDataFrame-class
#' @aliases SpectraDataFrame,spectra.data.frame
#' @docType class
#' @slot spectra A matrix
#' @slot wavelength A numeric vector
#' @slot w.unit A character string
#' @slot data A data.frame of vegetation data corresponding to the spectra
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
            .Object <- methods::callNextMethod()
            if(nrow(.Object@spectra) != nrow(.Object@data) && length(.Object@wavelength) >1)
              stop("specified 'spectra' and 'data' of different lengths")
            .Object
          }
)
#' Create a SpectraDataFrame
#'
#' Constructor \code{as.spectra.data.frame} function creates a SpectraDataFrame object, which is equivalent to the use of \link{as.specdf}.
#'
#' @name as.spectra.data.frame
#' @rdname SpectraDataFrame
#' @aliases as.specdf
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
  sls <- methods::new("SpectraDataFrame", spectra, wavelength, w.unit, data)
  spectra <- sls@spectra
  colnames(spectra) <- paste(wavelength, w.unit)
  sdf <- sls@data
  sdf$spectra <- spectra
  sdf
}

=======
#' @include visa.R
#'
#' Class 'Spectra'/'SpectraDatabase'
#'
#' An S4 Class `Spectra`, with five slots of data.
#' It has `SpectraDatabase` as the alias.
#'
#' @name Spectra-class
#' @rdname Spectra-class
#' @aliases Spectra,Spectra-class
#' @docType class
#' @slot spectra A matrix
#' @slot wavelength A numeric vector
#' @slot w.unit A character string
#' @slot data A data.frame
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
#' Class 'SpectraDatabase'
#'
#' SpectraDatabase is an extended 'Spectra' class, with associated vegetation data ('data')
#' in a \link{data.frame}.
#'
#' @name SpectraDatabase-class
#' @rdname SpectraDatabase-class
#' @aliases SpectraDatabase-class,spectra.database
#' @docType class
#' @slot spectra A matrix
#' @slot wavelength A numeric vector
#' @slot w.unit A character string
#' @slot data A data.frame of vegetation data corresponding to the spectra
#' @export
#' @exportClass SpectraDatabase
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
            .Object <- methods::callNextMethod()
            if(nrow(.Object@spectra) != nrow(.Object@data) && length(.Object@wavelength) >1)
              stop("specified 'spectra' and 'data' of different lengths")
            .Object
          }
)

#' Create a Spectra or SpectraDatabase
#'
#' Constructor \code{as.spectra} creates a Spectra object.
#'
#' @name as.spectra
#' @rdname Spectra-class
#' @param spectra A matrix
#' @param wavelength A numeric vector
#' @param w.unit A character string
#' @param data A data.frame
#' @param ... Other parameters
#' @examples
#' s <- as.spectra(matrix(1:100, 4), 1:25, "nm", data.frame(x = letters[1:4]))
#' str(s)
#'
#' @export
as.spectra <- function(spectra = matrix(0),
                       wavelength = numeric(0),
                       w.unit = "nm",
                       data = data.frame(), ...){
  return(methods::new("Spectra", spectra, wavelength, w.unit, data, ...))
}
#' Create a Spectra or SpectraDatabase
#'
#' Constructor \code{as.spectra.database} creates a SpectraDatabase object.
#'
#' @rdname Spectra-class
#' @examples
#' s <- as.spectra.database(matrix(1:100, 4), 1:25, "nm", data.frame(x = letters[1:4]))
#' str(s)
#' @export
as.spectra.database <- function(spectra = matrix(0),
                                wavelength = numeric(0),
                                w.unit = "nm",
                                data = data.frame(), ...){
  return(methods::new("Spectra", spectra, wavelength, w.unit, data, ...))
}


#' Class 'SpectraMatrix'
#'
#' SpectraMatrix is a extended 'Spectra' class.
#'
#' @name SpectraMatrix-class
#' @rdname SpectraMaxtrix-class
#' @aliases SpectraMaxtrix-class,spectra.maxtrix
#' @docType class
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
            .Object <- methods::callNextMethod()
            if(ncol(.Object@spectra) != length(.Object@wavelength))
              stop("specified spectra and wavelength are of different lengths")
            .Object
          }
)
#' Create a SpectraMatrix
#'
#' Constructor \code{as.spectra.matrix} creates a SpectraMatrix object.
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
  sls <- methods::new("SpectraMatrix", spectra, wavelength, w.unit)
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
#' @name SpectraDataFrame-class
#' @rdname SpectraDataFrame-class
#' @aliases SpectraDataFrame,spectra.data.frame
#' @docType class
#' @slot spectra A matrix
#' @slot wavelength A numeric vector
#' @slot w.unit A character string
#' @slot data A data.frame of vegetation data corresponding to the spectra
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
            .Object <- methods::callNextMethod()
            if(nrow(.Object@spectra) != nrow(.Object@data) && length(.Object@wavelength) >1)
              stop("specified 'spectra' and 'data' of different lengths")
            .Object
          }
)
#' Create a SpectraDataFrame
#'
#' Constructor \code{as.spectra.data.frame} function creates a SpectraDataFrame object, which is equivalent to the use of \link{as.specdf}.
#'
#' @name as.spectra.data.frame
#' @rdname SpectraDataFrame
#' @aliases as.specdf
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
  sls <- methods::new("SpectraDataFrame", spectra, wavelength, w.unit, data)
  spectra <- sls@spectra
  colnames(spectra) <- paste(wavelength, w.unit)
  sdf <- sls@data
  sdf$spectra <- spectra
  sdf
}

>>>>>>> fdcc887ca45316c73fbe6ef70768b77deffbc575
