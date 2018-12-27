#' Binomial regression with logistic link density (univariate, discrete, binary space)
#'
#' @inherit Density
#' @param xBeta Either a fixed value or a prior density for the parameter of the regression.
#' @param M     An integer with the number of covariates in the observation regression model.
#' @param N     An integer with the number of trials (fixed quantity).
#' @family Density
#' @examples
#' RegBinomialLogit(
#'   xBeta = Gaussian(0, 10),
#'   M     = 3,
#'   N     = 10
#' )
RegBinomialLogit <- function(xBeta = NULL, M = NULL, N = NULL, bounds = list(NULL, NULL),
                              trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("RegBinomialLogit", bounds, trunc, k, r, param, xBeta = xBeta, M = M, N = N)
}

#' @keywords internal
#' @inherit constants
constants.RegBinomialLogit <- function(x) {
  sprintf(
    "int<lower = 1> N = %s; // number of trials",
    x$N
  )
}

#' @keywords internal
#' @inherit block_data
block_data.RegBinomialLogit <- function(x, noLogLike) {
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
freeParameters.RegBinomialLogit <- function(x) {
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
fixedParameters.RegBinomialLogit <- function(x) {
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
generated.RegBinomialLogit <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = binomial_rng(N, inv_logit(x[t] * xBeta%s%s));",
    x$k, x$r,
    x$k, x$r
  )
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.RegBinomialLogit <- function(x) {
  return(c("xBeta"))
}

#' @keywords internal
#' @inherit logLike
logLike.RegBinomialLogit <- function(x) {
  sprintf(
    "loglike[%s][t] = binomial_logit_lpmf(y[t] | N, x[t] * xBeta%s%s);",
    x$k,
    x$k, x$r
  )
}

#' @keywords internal
#' @inherit prior
prior.RegBinomialLogit <- function(x) {
  stop("Not to be used as a prior :)")
}
