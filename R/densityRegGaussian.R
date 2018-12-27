#' Regression with Gaussian link density (univariate, continuous, unbounded space)
#'
#' @inherit Density
#' @param sigma Either a fixed value or a prior density for the shape parameter.
#' @param xBeta Either a fixed value or a prior density for the parameter of the regression.
#' @param M     An integer with the number of covariates in the observation regression model.
#' @family Density
#' @examples
#' RegGaussian(
#'   sigma = Cauchy(mu = 0, sigma = 10, bounds = list(0, NULL)),
#'   xBeta = Gaussian(0, 10),
#'   M     = 3
#' )
RegGaussian <- function(sigma = NULL, xBeta = NULL, M = NULL, bounds = list(NULL, NULL),
                     trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("RegGaussian", bounds, trunc, k, r, param, sigma = sigma, xBeta = xBeta, M = M)
}

#' @keywords internal
#' @inherit block_data
block_data.RegGaussian <- function(x, noLogLike) {
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
freeParameters.RegGaussian <- function(x) {
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

  sigmaStr <-
    if (is.Density(x$sigma)) {
      sigmaBoundsStr <- make_bounds(x, "sigma")
      sprintf(
        "real%s sigma%s%s;",
        sigmaBoundsStr, x$k, x$r
      )
    } else {
      ""
    }

  collapse(xBetaStr, sigmaStr)
}

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.RegGaussian <- function(x) {
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

  sigmaStr <-
    if (is.Density(x$sigma)) {
      ""
    } else {
      if (!check_scalar(x$sigma)) {
        stop("If fixed, sigma must be a scalar.")
      }

      sprintf(
        "real sigma%s%s = %s;",
        x$k, x$r, x$sigma
      )
    }

  collapse(xBetaStr, sigmaStr)
}

#' @keywords internal
#' @inherit generated
generated.RegGaussian <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = normal_rng(x[t] * xBeta%s%s, sigma%s%s);",
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.RegGaussian <- function(x) {
  return(c("xBeta", "sigma"))
}

#' @keywords internal
#' @inherit logLike
logLike.RegGaussian <- function(x) {
  sprintf(
    "loglike[%s][t] = normal_lpdf(y[t] | x[t] * xBeta%s%s, sigma%s%s);",
    x$k,
    x$k, x$r,
    x$k, x$r
  )
}

#' @keywords internal
#' @inherit prior
prior.RegGaussian <- function(x) {
  stop("Not to be used as a prior :)")
}
