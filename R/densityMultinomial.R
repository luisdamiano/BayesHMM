#' Multinomial mass (multivariate, discrete, bounded space)
#'
#' @inherit Density
#' @param theta Either a fixed value or a prior density for the success proportion vector parameter.
#' @param N     An integer with the number of trials (fixed quantity).
#'
#' @family Density
#'
#' @examples
#' # With fixed values for the parameters
#' Multinomial(c(0.1, 0.3, 0.6), 10)
#'
#' # With priors for the parameters
#' Multinomial(
#'   Dirichlet(c(1, 1, 1)), 10
#' )
Multinomial <- function(theta = NULL, N = NULL, ordered = NULL, equal = NULL, bounds = list(NULL, NULL),
                        trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity("Multinomial", ordered, equal, bounds, trunc, k, r, param, theta = theta, N = N)
}

#' @keywords internal
#' @inherit constants
constants.Multinomial <- function(x) {
  sprintf(
    "int<lower = 1> N = %s; // number of trials",
    x$N
  )
}

#' @keywords internal
#' @inherit freeParameters
freeParameters.Multinomial <- function(x) {
  thetaStr <-
    if (is.Density(x$theta)) {
      sprintf(
        "simplex[N] theta%s%s;",
        get_k(x, "theta"), get_r(x, "theta")
      )
    } else {
      ""
    }

  thetaStr
}

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.Multinomial <- function(x) {
  thetaStr <-
    if (is.Density(x$theta)) {
      ""
    } else {
      if (!check_simplex(x$theta)) {
        stop("If fixed, theta must be an unit simplex")
      }

      sprintf(
        "simplex[N] theta%s%s = %s';",
        get_k(x, "theta"), get_r(x, "theta"), vector_to_stan(x$theta)
      )
    }

  thetaStr
}

#' @keywords internal
#' @inherit generated
generated.Multinomial <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t] = to_row_vector(multinomial_rng(theta%s%s, N));",
    x$k,
    get_k(x, "theta"), get_r(x, "theta")
  )
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.Multinomial <- function(x) {
  return("theta")
}

#' @keywords internal
#' @inherit logLike
logLike.Multinomial <- function(x) {
  sprintf(
    "loglike[%s][t] = multinomial_lpmf(y[t] | theta%s%s);",
    x$k,
    get_k(x, "theta"), get_r(x, "theta")
  )
}

#' @keywords internal
#' @inherit prior
prior.Multinomial <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ multinomial(%s) %s;",
    x$param,
    x$k, rStr,
    x$theta,
    truncStr
  )
}
