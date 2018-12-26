#' Softmax transition density
#'
#' @inherit Density
#' @param uBeta Either a fixed value or a prior density for the parameter of the softmax regression.
#' @param P     An integer with the number of covariates in the transition model.
#' @family Density
#' @examples
#' TransitionSoftmax(
#'   uBeta = Gaussian(0, 10),
#'   P     = 3
#' )
TransitionSoftmax <- function(uBeta = NULL, P = NULL, bounds = list(NULL, NULL),
                           trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("TransitionSoftmax", bounds, trunc, k, r, param, uBeta = uBeta, P = P)
}

#' @inherit explain_density
explain_density.TransitionSoftmax <- function(x) {
  collapse(
    "Time-varying probabilities driven by covariates via softmax mapping.",
    NextMethod()
  )
}

#' @inherit block_data
block_data.TransitionSoftmax <- function(x, noLogLike) {
  c(
    "int<lower = 1> P;     // number of transition model predictors",
    "matrix[T, P] u;       // transition model predictors"
  )
}

#' @inherit freeParameters
freeParameters.TransitionSoftmax <- function(x) {
  uBetaStr <-
    if (is.Density(x$uBeta)) {
      uBetaBoundsStr <- make_bounds(x, "uBeta")
      sprintf(
        "
        matrix%s[K, P] uBeta[K];        // transition model regressors
                                        // uBeta[to, from, p regressors]
        ",
        uBetaBoundsStr
      )
    } else {
      ""
    }

  uBetaStr
}

#' @inherit fixedParameters
fixedParameters.TransitionSoftmax <- function(x) {
  warning("fixedParameters.Softmax: TO BE IMPLEMENTED.")
  return("")
}

#' @inherit getParameterNames
getParameterNames.TransitionSoftmax <- function(x) {
  return("uBeta")
}

#' @inherit link
link.TransitionSoftmax <- function(x) {
  sprintf(
    "A[t, i] = softmax((u[t] * uBeta%s[i]')');",
    x$k
  )
}

#' @inherit prior
prior.TransitionSoftmax <- function(x) {
  warning("prior.Softmax: TO BE IMPLEMENTED.")
  return("")
}
