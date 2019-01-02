#' Dirichlet density (multivariate, continuous, unbounded space)
#'
#' @inherit Density
#' @param alpha Either a fixed value or a prior density for the vector parameter.
#'
#' @family Density
#'
#' @examples
#' # With fixed values for the parameters
#' Dirichlet(alpha = c(0.5, 0.5, 0.5))
#'
#' # With priors for the parameters
#' Dirichlet(alpha = Beta(1, 1))
Dirichlet <- function(alpha = NULL, ordered = NULL, bounds = list(NULL, NULL),
                      trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity("Dirichlet", ordered, bounds, trunc, k, r, param, alpha = alpha)
}

#' @keywords internal
#' @inherit freeParameters
freeParameters.Dirichlet <- function(x) {
  ""
  # stop("TO BE IMPLEMENTED.")
}

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.Dirichlet <- function(x) {
  ""
  # stop("TO BE IMPLEMENTED.")
}

#' @keywords internal
#' @inherit generated
generated.Dirichlet <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.Dirichlet <- function(x) {
  return(c("alpha"))
}

#' @keywords internal
#' @inherit logLike
logLike.Dirichlet <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

#' @keywords internal
#' @inherit prior
prior.Dirichlet <- function(x) {
  sprintf("%s%s%s ~ dirichlet(%s);", x$param, x$k, x$r,
          sprintf("[%s]'", paste(eval(x$alpha), collapse = ", ")))
}
