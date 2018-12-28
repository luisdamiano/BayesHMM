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
  LinkDensity("TransitionSoftmax", bounds, trunc, k, r, param, uBeta = uBeta, P = P)
}

#' @keywords internal
#' @inherit explain_density
explain_density.TransitionSoftmax <- function(x) {
  collapse(
    "Time-varying probabilities driven by covariates via softmax mapping.",
    NextMethod()
  )
}

#' @keywords internal
#' @inherit freeParameters
freeParameters.TransitionSoftmax <- function(x) {
  uBetaStr <-
    if (is.Density(x$uBeta)) {
      uBetaBoundsStr <- make_bounds(x, "uBeta")
      sprintf(
        "
        matrix%s[K, P] uBeta[K];        // transition model regressors
                                        // uBeta[to, from, p-th regressor]
        ",
        uBetaBoundsStr
      )
    } else {
      ""
    }

  uBetaStr
}

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.TransitionSoftmax <- function(x) {
  warning("fixedParameters.Softmax: TO BE IMPLEMENTED.")
  ""
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.TransitionSoftmax <- function(x) {
  "uBeta"
}

#' @keywords internal
#' @inherit is.TVTransition
is.TVTransition.TransitionSoftmax <- function(x) { TRUE }

#' @keywords internal
#' @inherit link
link.TransitionSoftmax <- function(x) {
  sprintf(
    "A[t, i] = softmax((u[t] * uBeta%s[i]')');",
    x$k
  )
}

#' @keywords internal
#' @inherit prior
prior.TransitionSoftmax <- function(x) { "" }
