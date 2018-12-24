#' Exponential density (univariate, continuous, bounded space)
#'
#' @inherit Density
#' @param beta Either a fixed value or a prior density for the inverse scale parameter.
#'
#' @family Density
#' #'
#'
#' @examples
#' # With fixed values for the parameters
#' Exponential(1)
#'
#' # With priors for the parameters
#' Exponential(Exponential(1))
Exponential <- function(beta = NULL, bounds = list(NULL, NULL),
                        trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("Exponential", bounds, trunc, k, r, param, beta = beta)
}

freeParameters.Exponential <- function(x) {
  betaStr <-
    if (is.Density(x$beta)) {
      betaBoundsStr <- make_bounds(x, "beta")
      sprintf(
        "real%s beta%s%s;",
        betaBoundsStr, x$k, x$r
      )
    } else {
      ""
    }

  betaStr
}

fixedParameters.Exponential <- function(x) {
  betaStr <-
    if (is.Density(x$beta)) {
      ""
    } else {
      if (!check_scalar(x$beta)) {
        stop("If fixed, beta must be a scalar.")
      }

      sprintf(
        "real beta%s%s = %s;",
        x$k, x$r, x$beta
      )
    }

  betaStr
}

generated.Exponential <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = exponential_rng(beta%s%s);",
    x$k, x$r,
    x$k, x$r
  )
}

getParameterNames.Exponential <- function(x) {
  return("beta")
}

logLike.Exponential <- function(x) {
  sprintf(
    "loglike[%s][t] = exponential_lpdf(y[t] | beta%s%s);",
    x$k,
    x$k, x$r
  )
}

prior.Exponential <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ exponential(%s) %s;",
    x$param,
    x$k, rStr,
    x$beta,
    truncStr
  )
}
