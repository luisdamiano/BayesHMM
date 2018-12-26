# extract_n_chains ------------------------------------------------------------
#' Extract the number of chains (M).
#'
#' @param stanfit An object returned by either \code{\link{fit}} or \code{\link{drawSamples}}.
#' @return The number of chains (M) used to fit the model.
#' #'Method extract_n_chains
setGeneric("extract_n_chains", function(stanfit) {standardGeneric("extract_n_chains")} )

#' @rdname extract_n_chains
#' @aliases extract_n_chains,stanfit-method
setMethod("extract_n_chains", "stanfit", function(stanfit) {
  stanfit@sim$chains
})

#' Check if it is an object created by \code{\link{drawSamples}}.
#'
#' @keywords internal
#' @param x An object.
#' @return TRUE if it is an object created by \code{\link{drawSamples}}.
is.stanfit <- function(x) {
  class(x) == "stanfit"
}
