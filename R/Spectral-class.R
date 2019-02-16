#' @include visa.R
#'
#' #' To be further developed ...
#'
#' An S4 class to represent a SpectralDataFrame.
#'
#' @slot entry A dataframe, with as many rows as observations (samples).
#' @slot spec A numeric matrix, with as many columns as wavelengths.
#' @slot wavelength A numeric vector
#' @slot wunit A character stringy
#' @export
#' @exportClass SpectralDataFrame

setClass("SpectralDataFrame",
         slots = c(entry = "data.frame",
                   spec = "matrix",
                   wavelength = "vector",
                   wunit = "character")
)

setValidity("SpectralDataFrame",
            function(object)
            {
              if (length(object@entry) != 1)
                return("'genome' slot must have length 1")
              slot_lengths <- c(length(object@spec),
                                length(object@wavelength),
                                length(object@wunit))
              if (length(unique(slot_lengths)) != 1)
                return("'spec', 'wavelength' and 'wunit' slots must have the same length")
              TRUE
            }
)
