#' Poisson mass (univariate, discrete, bounded space)
#'
#' @inherit Density
#' @param lambda Either a fixed value or a prior density for the rate parameter.
#'
#' @family Density
#'
#' @examples
#' # With fixed values for the parameters
#' Poisson(5)
#'
#' # With priors for the parameters
#' Poisson(GammaDensity(1, 1))
Poisson <- function(lambda = NULL, bounds = list(NULL, NULL),
                    trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity("Poisson", bounds, trunc, k, r, param, lambda = lambda)
}

#' @keywords internal
#' @inherit freeParameters
freeParameters.Poisson <- function(x) {
  lambdaStr <-
    if (is.Density(x$lambda)) {
      lambdaBoundsStr <- make_bounds(x, "lambda")
      sprintf(
        "real%s lambda%s%s;",
        lambdaBoundsStr, x$k, x$r
      )
    } else {
      ""
    }

  lambdaStr
}

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.Poisson <- function(x) {
  lambdaStr <-
    if (is.Density(x$lambda)) {
      ""
    } else {
      if (!check_scalar(x$lambda)) {
        stop("If fixed, lambda must be a scalar.")
      }

      sprintf(
        "real lambda%s%s = %s;",
        x$k, x$r, x$lambda
      )
    }

  lambdaStr
}

#' @keywords internal
#' @inherit generated
generated.Poisson <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = poisson_rng(lambda%s%s);",
    x$k, x$r,
    x$k, x$r
  )
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.Poisson <- function(x) {
  return("lambda")
}

#' @keywords internal
#' @inherit logLike
logLike.Poisson <- function(x) {
  sprintf(
    "loglike[%s][t] = poisson_lpmf(y[t] | lambda%s%s);",
    x$k,
    x$k, x$r
  )
}

#' @keywords internal
#' @inherit prior
prior.Poisson <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ poisson(%s) %s;",
    x$param,
    x$k, rStr,
    x$lambda,
    truncStr
  )
}
