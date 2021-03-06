#' Binomial mass (univariate, discrete, bounded space)
#'
#' @inherit Density
#' @param theta Either a fixed value or a prior density for the success proportion parameter.
#' @param N     An integer with the number of trials (fixed quantity).
#' @family Density
#' @examples
#' # With fixed values for the parameters
#' Binomial(0.5, 10)
#'
#' # With priors for the parameters
#' Binomial(
#'   Beta(1, 1), 10
#' )
Binomial <- function(theta = NULL, N = NULL, ordered = NULL, equal = NULL, bounds = list(NULL, NULL),
                     trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity("Binomial", ordered, equal, bounds, trunc, k, r, param, theta = theta, N = N)
}

#' @keywords internal
#' @inherit constants
constants.Binomial <- function(x) {
  sprintf(
    "int<lower = 1> N = %s; // number of trials",
    x$N
  )
}

#' @keywords internal
#' @inherit freeParameters
freeParameters.Binomial <- function(x) {
  thetaStr <-
    if (is.Density(x$theta)) {
      thetaBoundsStr <- make_bounds(x, "theta")
      sprintf(
        "real%s theta%s%s;",
        thetaBoundsStr, get_k(x, "theta"), get_r(x, "theta")
      )
    } else {
      ""
    }

  thetaStr
}

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.Binomial <- function(x) {
  thetaStr <-
    if (is.Density(x$theta)) {
      ""
    } else {
      if (!check_scalar(x$theta)) {
        stop("If fixed, theta must be a scalar.")
      }

      sprintf(
        "real theta%s%s = %s;",
        get_k(x, "theta"), get_r(x, "theta"), x$theta
      )
    }

  thetaStr
}

#' @keywords internal
#' @inherit generated
generated.Binomial <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = binomial_rng(N, theta%s%s);",
    x$k, x$r,
    get_k(x, "theta"), get_r(x, "theta")
  )
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.Binomial <- function(x) {
  return("theta")
}

#' @keywords internal
#' @inherit logLike
logLike.Binomial <- function(x) {
  sprintf(
    "loglike[%s][t] = binomial_lpmf(y[t] | N, theta%s%s);",
    x$k,
    get_k(x, "theta"), get_r(x, "theta")
  )
}

#' @keywords internal
#' @inherit prior
prior.Binomial <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ binomial(N, %s) %s;",
    x$param,
    x$k, rStr,
    x$theta,
    truncStr
  )
}
