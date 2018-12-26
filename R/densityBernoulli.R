#' Bernoulli mass (univariate, discrete, bounded space)
#'
#' @inherit Density
#' @param theta Either a fixed value or a prior density for the success proportion parameter.
#'
#' @family Density
#' #'
#'
#' @examples
#' # With fixed values for the parameters
#' Bernoulli(0.5)
#'
#' # With priors for the parameters
#' Bernoulli(
#'   Beta(1, 1)
#' )
Bernoulli <- function(theta = NULL, bounds = list(NULL, NULL),
                      trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity("Bernoulli", bounds, trunc, k, r, param, theta = theta)
}

#' @inherit freeParameters
freeParameters.Bernoulli <- function(x) {
  thetaStr <-
    if (is.Density(x$theta)) {
      thetaBoundsStr <- make_bounds(x, "theta")
      sprintf(
        "real%s theta%s%s;",
        thetaBoundsStr, x$k, x$r
      )
    } else {
      ""
    }

  thetaStr
}

#' @inherit fixedParameters
fixedParameters.Bernoulli <- function(x) {
  thetaStr <-
    if (is.Density(x$theta)) {
      ""
    } else {
      if (!check_scalar(x$theta)) {
        stop("If fixed, theta must be a scalar.")
      }

      sprintf(
        "real theta%s%s = %s;",
        x$k, x$r, x$theta
      )
    }

  thetaStr
}

#' @inherit generated
generated.Bernoulli <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = bernoulli_rng(theta%s%s);",
    x$k, x$r,
    x$k, x$r
  )
}

#' @inherit getParameterNames
getParameterNames.Bernoulli <- function(x) {
  return("theta")
}

#' @inherit logLike
logLike.Bernoulli <- function(x) {
  sprintf(
    "loglike[%s][t] = bernoulli_lpmf(y[t] | theta%s%s);",
    x$k,
    x$k, x$r
  )
}

#' @inherit prior
prior.Bernoulli <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ bernoulli(%s) %s;",
    x$param,
    x$k, rStr,
    x$theta,
    truncStr
  )
}
