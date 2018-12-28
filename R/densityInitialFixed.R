#' Fixed initial probability vector
#'
#' @inherit Density
#' @param pi A numeric vector of size \emph{K} with elements suming to one.
#' @family Density
#' @examples
#' InitialFixed(
#'   pi = c(0.5, 0.2, 0.3)
#' )
InitialFixed   <- function(pi = NULL, bounds = list(NULL, NULL),
                              trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  LinkDensity("InitialFixed", bounds, trunc, k, r, param, pi = pi)
}

#' @keywords internal
#' @inherit explain_density
explain_density.InitialFixed     <- function(x) {
  sprintf(
    "Fixed vector: %s.",
    vector_to_stan(x$pi)
  )
}

#' @keywords internal
#' @inherit freeParameters
freeParameters.InitialFixed     <- function(x) { "" }

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.InitialFixed    <- function(x) { "" }

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.InitialFixed  <- function(x) { "" }

#' @keywords internal
#' @inherit is.FixedInitial
is.FixedInitial.InitialFixed    <- function(x) { TRUE }

#' @keywords internal
#' @inherit link
link.InitialFixed <- function(x) {
  if (!(check_simplex(x$pi) || length(x$pi) != x$K) ) {
    stop("If fixed, pi must be a simplex of size K.")
  }

  sprintf(
    "pi = %s';",
    vector_to_stan(x$pi)
  )
}

#' @keywords internal
#' @inherit prior
prior.InitialFixed              <- function(x) { "" }
