# extract_n_chains ------------------------------------------------------------
#' Extract the number of chains (M).
#'
#' @param stanfit An object returned by either \code{\link{fit}} or \code{\link{sampling}}.
#' @return The number of chains (M) used to fit the model.
#' @export
#' @examples
setGeneric("extract_n_chains", function(stanfit) {standardGeneric("extract_n_chains")} )

setMethod("extract_n_chains", "stanfit", function(stanfit) {
  stanfit@sim$chains
})

#' Check if it is an object created by \code{\link{sampling}}.
#'
#' @keywords internal
#' @param x An object.
#' @return TRUE if it is an object created by \code{\link{sampling}}.
#' @examples
is.stanfit <- function(x) {
  class(x) == "stanfit"
}
