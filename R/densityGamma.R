#' Gamma density (univariate, continuous, bounded space)
#'
#' @inherit Density
#' @param alpha Either a fixed value or a prior density for the shape parameter.
#' @param beta  Either a fixed value or a prior density for the inverse scale parameter.
#'
#' @family Density
#'
#' @examples
#' # With fixed values for the parameters
#' GammaDensity(1, 1)
#'
#' # With priors for the parameters
#' GammaDensity(
#'   alpha = Exponential(1), beta = Exponential(1)
#' )
GammaDensity <- function(alpha = NULL, beta = NULL, ordered = NULL, equal = NULL, bounds = list(NULL, NULL),
                 trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("GammaDensity", ordered, equal, bounds, trunc, k, r, param, alpha = alpha, beta = beta)
}

#' @keywords internal
#' @inherit freeParameters
freeParameters.GammaDensity <- function(x) {
  alphaStr <-
    if (is.Density(x$alpha)) {
      alphaBoundsStr <- make_bounds(x, "alpha")
      sprintf(
        "real%s alpha%s%s;",
        alphaBoundsStr, get_k(x, "alpha"), get_r(x, "alpha")
      )
    } else {
      ""
    }

  betaStr <-
    if (is.Density(x$beta)) {
      betaBoundsStr <- make_bounds(x, "beta")
      sprintf(
        "real%s beta%s%s;",
        betaBoundsStr, get_k(x, "beta"), get_r(x, "beta")
      )
    } else {
      ""
    }

  collapse(alphaStr, betaStr)
}

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.GammaDensity <- function(x) {
  alphaStr <-
    if (is.Density(x$alpha)) {
      ""
    } else {
      if (!check_scalar(x$alpha)) {
        stop("If fixed, alpha must be a scalar.")
      }

      sprintf(
        "real alpha%s%s = %s;",
        get_k(x, "alpha"), get_r(x, "alpha"), x$alpha
      )
    }

  betaStr <-
    if (is.Density(x$beta)) {
      ""
    } else {
      if (!check_scalar(x$beta)) {
        stop("If fixed, beta must be a scalar.")
      }

      sprintf(
        "real beta%s%s = %s;",
        get_k(x, "beta"), get_r(x, "beta"), x$beta
      )
    }

  collapse(alphaStr, betaStr)
}

#' @keywords internal
#' @inherit generated
generated.GammaDensity <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = gamma_rng(alpha%s%s, beta%s%s);",
    x$k, x$r,
    get_k(x, "alpha"), get_r(x, "alpha"),
    get_k(x, "beta"), get_r(x, "beta")
  )
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.GammaDensity <- function(x) {
  return(c("alpha", "beta"))
}

#' @keywords internal
#' @inherit logLike
logLike.GammaDensity <- function(x) {
  sprintf(
    "loglike[%s][t] = gamma_lpdf(y[t] | alpha%s%s, beta%s%s);", x$k,
    get_k(x, "alpha"), get_r(x, "alpha"),
    get_k(x, "beta"), get_r(x, "beta")
  )
}

#' @keywords internal
#' @inherit prior
prior.GammaDensity <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ gamma(%s, %s) %s;",
    x$param,
    x$k, rStr,
    x$alpha, x$beta,
    truncStr
  )
}
