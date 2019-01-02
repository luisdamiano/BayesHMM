#' Improper uniform prior density (prior only)
#'
#' Improper uniform prior.
#'
#' @inherit Density
#'
#' @family Density
#'
#' @examples
#' # As a prior
#' Beta(ImproperUniform(), ImproperUniform())
ImproperUniform <- function(ordered = NULL, bounds = list(NULL, NULL),
                    trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  PriorOnlyDensity("ImproperUniform", ordered, bounds, trunc, k, r, param)
}

#' @keywords internal
#' @inherit prior
prior.ImproperUniform <- function(x) {
  sprintf("// %s%s%s ~ Improper Uniform Prior;", x$param, x$k, x$r)
}
