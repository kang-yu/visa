#' @include visa.R
#'
#' @export
setMethod("length", "SpectralDataFrame", function(x) length(x@wavelength))
