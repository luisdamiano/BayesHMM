#' Categorical regression with softmax link density (univariate, discrete, bounded space)
#'
#' @inherit Density
#' @param xBeta Either a fixed value or a prior density for the parameter of the regression.
#' @param M     An integer with the number of covariates in the observation regression model.
#' @param N     An integer with the number of trials (fixed quantity).
#' @family Density
#' @examples
#' RegCategoricalSoftmax(
#'   xBeta = Gaussian(0, 10),
#'   M     = 3,
#'   N     = 10
#' )
RegCategoricalSoftmax <- function(xBeta = NULL, M = NULL, N = NULL, ordered = NULL, bounds = list(NULL, NULL),
                             trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity("RegCategoricalSoftmax", ordered, bounds, trunc, k, r, param, xBeta = xBeta, M = M, N = N)
}

#' @keywords internal
#' @inherit constants
constants.RegCategoricalSoftmax <- function(x) {
  sprintf(
    "int<lower = 1> N = %s; // number of categories",
    x$N
  )
}

#' @keywords internal
#' @inherit block_data
block_data.RegCategoricalSoftmax <- function(x, noLogLike) {
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
freeParameters.RegCategoricalSoftmax <- function(x) {
  xBetaStr <-
    if (is.Density(x$xBeta)) {
      # xBetaBoundsStr <- make_bounds(x, "xBeta")
      # sprintf(
      #   "vector%s[M] xBeta%s%s;",
      #   xBetaBoundsStr, x$k, x$r
      # )
      xBetaBoundsStr <- make_bounds(x, "xBeta")
      sprintf(
        "matrix%s[N, M] xBeta%s%s;",
        xBetaBoundsStr, x$k, x$r
      )
    } else {
      ""
    }

  xBetaStr
}

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.RegCategoricalSoftmax <- function(x) {
  xBetaStr <-
    if (is.Density(x$xBeta)) {
      ""
    } else {
      if (!check_matrix(x$xBeta)) {
        stop("If fixed, xBeta must be a matrix.")
      }

      sprintf(
        "matrix[N, M] xBeta%s%s = %s;",
        x$k, x$r, x$xBeta
      )
    }

  xBetaStr
}

#' @keywords internal
#' @inherit generated
generated.RegCategoricalSoftmax <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = categorical_logit_rng((x[t] * xBeta%s%s')');",
    x$k, x$r,
    x$k, x$r
  )
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.RegCategoricalSoftmax <- function(x) {
  return(c("xBeta"))
}

#' @keywords internal
#' @inherit logLike
logLike.RegCategoricalSoftmax <- function(x) {
  sprintf(
    "loglike[%s][t] = categorical_logit_lpmf(y[t] | (x[t] * xBeta%s%s')');",
    x$k,
    x$k, x$r
  )
}

#' @keywords internal
#' @inherit prior
prior.RegCategoricalSoftmax <- function(x) {
  stop("Not to be used as a prior :)")
}
