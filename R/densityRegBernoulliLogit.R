#' Bernoulli regression with logistic link density (univariate, discrete, binary space)
#'
#' @inherit Density
#' @param xBeta Either a fixed value or a prior density for the parameter of the regression.
#' @param M     An integer with the number of covariates in the observation regression model.
#' @family Density
#' @examples
#' RegBernoulliLogit(
#'   xBeta = Gaussian(0, 10),
#'   M     = 3
#' )
RegBernoulliLogit <- function(xBeta = NULL, M = NULL, bounds = list(NULL, NULL),
                        trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("RegBernoulliLogit", bounds, trunc, k, r, param, xBeta = xBeta, M = M)
}

#' @inherit block_data
block_data.RegBernoulliLogit <- function(x, noLogLike) {
  c(
    "int<lower = 1> M; // number of predictors",
    "matrix[T, M] x;   // predictors",
    if (!noLogLike) { NextMethod()}
  )
}

#' @inherit freeParameters
freeParameters.RegBernoulliLogit <- function(x) {
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

#' @inherit fixedParameters
fixedParameters.RegBernoulliLogit <- function(x) {
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

#' @inherit generated
generated.RegBernoulliLogit <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = bernoulli_logit_rng(x[t] * xBeta%s%s);",
    x$k, x$r,
    x$k, x$r
  )
}

#' @inherit getParameterNames
getParameterNames.RegBernoulliLogit <- function(x) {
  return(c("xBeta"))
}

#' @inherit logLike
logLike.RegBernoulliLogit <- function(x) {
  sprintf(
    "loglike[%s][t] = bernoulli_logit_lpmf(y[t] | x[t] * xBeta%s%s);",
    x$k,
    x$k, x$r
  )
}

#' @inherit prior
prior.RegBernoulliLogit <- function(x) {
  stop("Not to be used as a prior :)")
}
