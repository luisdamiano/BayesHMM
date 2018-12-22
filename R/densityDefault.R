#' Default prior density (prior only)
#'
#' Improper uniform prior.
#'
#' @inherit Density
#'
#' @family Density
#' @export
#'
#' @examples
#' # As a prior
#' Beta(Default(), Default())
Default <- function(bounds = list(NULL, NULL),
                    trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  PriorOnlyDensity("Default", bounds, trunc, k, r, param)
}

prior.Default <- function(x) {
  sprintf("// %s%s%s ~ Improper Uniform Prior;", x$param, x$k, x$r)
}
