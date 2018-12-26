#' Softmax initial probability density
#'
#' @inherit Density
#' @param vBeta Either a fixed value or a prior density for the parameter of the softmax regression.
#' @param Q     An integer with the number of covariates in the initial distribution model.
#' @family Density
#' @examples
#' InitialSoftmax(
#'   vBeta = Gaussian(0, 10),
#'   Q     = 3
#' )
InitialSoftmax <- function(vBeta = NULL, Q = NULL, bounds = list(NULL, NULL),
                    trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("InitialSoftmax", bounds, trunc, k, r, param, vBeta = vBeta, Q = Q)
}

#' @inherit freeParameters
freeParameters.InitialSoftmax <- function(x) {
  vBetaStr <-
    if (is.Density(x$vBeta)) {
      vBetaBoundsStr <- make_bounds(x, "vBeta")
      sprintf(
        "
        matrix%s[K, Q] vBeta;   // initial model regressors
                                // vBeta[state, Q regressors]
        ",
        vBetaBoundsStr
      )
    } else {
      ""
    }

  vBetaStr
}

#' @inherit fixedParameters
fixedParameters.InitialSoftmax <- function(x) {
  warning("fixedParameters.Softmax: TO BE IMPLEMENTED.")
  return("")
}

#' @inherit getParameterNames
getParameterNames.InitialSoftmax <- function(x) {
  return("vBeta")
}

#' @inherit link
link.InitialSoftmax <- function(x) {
  sprintf(
    "pi = softmax((v%s' * vBeta%s)');",
    x$k, x$k
  )
}
