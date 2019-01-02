#' Binomial regression with probit link density (univariate, discrete, bounded space)
#'
#' @inherit Density
#' @param xBeta Either a fixed value or a prior density for the parameter of the regression.
#' @param M     An integer with the number of covariates in the observation regression model.
#' @param N     An integer with the number of trials (fixed quantity).
#' @family Density
#' @examples
#' RegBinomialProbit(
#'   xBeta = Gaussian(0, 10),
#'   M     = 3,
#'   N     = 10
#' )
RegBinomialProbit <- function(xBeta = NULL, M = NULL, N = NULL, ordered = NULL, bounds = list(NULL, NULL),
                             trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity("RegBinomialProbit", ordered, bounds, trunc, k, r, param, xBeta = xBeta, M = M, N = N)
}

#' @keywords internal
#' @inherit constants
constants.RegBinomialProbit <- function(x) {
  sprintf(
    "int<lower = 1> N = %s; // number of trials",
    x$N
  )
}

#' @keywords internal
#' @inherit block_data
block_data.RegBinomialProbit <- function(x, noLogLike) {
  collapse(
    c(
      "int<lower = 1> M; // number of predictors",
      "matrix[T, M] x;   // predictors",
      NextMethod()
    )
  )
}

#' @keywords internal
#' @inherit freeParameters
freeParameters.RegBinomialProbit <- function(x) {
  xBetaStr <-
    if (is.Density(x$xBeta)) {
      xBetaBoundsStr <- make_bounds(x, "xBeta")
      sprintf(
        "vector%s[M] xBeta%s%s;",
        xBetaBoundsStr, x$k, x$r
      )
    } else {
      ""
    }

  xBetaStr
}

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.RegBinomialProbit <- function(x) {
  xBetaStr <-
    if (is.Density(x$xBeta)) {
      ""
    } else {
      if (!check_vector(x$xBeta)) {
        stop("If fixed, xBeta must be a vector.")
      }

      sprintf(
        "vector[M] xBeta%s%s = %s;",
        x$k, x$r, x$xBeta
      )
    }

  xBetaStr
}

#' @keywords internal
#' @inherit generated
generated.RegBinomialProbit <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = binomial_rng(N, Phi(x[t] * xBeta%s%s));",
    x$k, x$r,
    x$k, x$r
  )
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.RegBinomialProbit <- function(x) {
  return(c("xBeta"))
}

#' @keywords internal
#' @inherit logLike
logLike.RegBinomialProbit <- function(x) {
  sprintf(
    "loglike[%s][t] = binomial_lpmf(y[t] | N, Phi(x[t] * xBeta%s%s));",
    x$k,
    x$k, x$r
  )
}

#' @keywords internal
#' @inherit prior
prior.RegBinomialProbit <- function(x) {
  stop("Not to be used as a prior :)")
}
