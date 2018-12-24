#' Categorical mass (univariate, discrete, bounded space)
#'
#' @inherit Density
#' @param theta Either a fixed value or a prior density for the success proportion vector parameter.
#' @param N     An integer with the number of trials (fixed quantity).
#'
#' @family Density
#' #'
#'
#' @examples
#' # With fixed values for the parameters
#' Categorical(
#'   theta = c(0.2, 0.4, 0.1, 0.3),
#'   N = 4
#' )
#'
#' # With priors for the parameters
#' Categorical(
#'   theta = Dirichlet(alpha = c(1, 1, 1, 1)),
#'   N = 4
#' )
Categorical <- function(theta = NULL, N = NULL, bounds = list(NULL, NULL),
                        trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity("Categorical", bounds, trunc, k, r, param, theta = theta, N = N)
}

constants.Categorical <- function(x) {
  sprintf(
    "int<lower = 1> N = %s; // number of possible outcomes (discrete categories)",
    x$N
  )
}

freeParameters.Categorical <- function(x) {
  thetaStr <-
    if (is.Density(x$theta)) {
      sprintf(
        "simplex[N] theta%s%s;",
        x$k, x$r
      )
    } else {
      ""
    }

  thetaStr
}

fixedParameters.Categorical <- function(x) {
  thetaStr <-
    if (is.Density(x$theta)) {
      ""
    } else {
      if (!check_simplex(x$theta)) {
        stop("If fixed, theta must be an unit simplex.")
      }

      sprintf(
        # "real<lower = 0, upper = 1> theta%s%s = %s;",
        "simplex[N] theta%s%s = %s';",
        x$k, x$r, vector_to_stan(x$theta)
      )
    }

  thetaStr
}

generated.Categorical <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = categorical_rng(theta%s%s);",
    x$k, x$r,
    x$k, x$r
  )
}

getParameterNames.Categorical <- function(x) {
  return("theta")
}

logLike.Categorical <- function(x) {
  sprintf(
    "loglike[%s][t] = categorical_lpmf(y[t] | theta%s%s);",
    x$k,
    x$k, x$r
  )
}

prior.Categorical <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ categorical(%s) %s;",
    x$param,
    x$k, rStr,
    x$theta,
    truncStr
  )
}
