#' Negative Binomial mass (univariate, discrete, bounded space)
#'
#' @inherit Density
#' @param alpha Either a fixed value or a prior density for the first shape parameter.
#' @param beta  Either a fixed value or a prior density for the second shape parameter.
#'
#' @family Density
#' #'
#'
#' @examples
#' # With fixed values for the parameters
#' NegativeBinomial(1, 2, 10)
#'
#' # With priors for the parameters
#' NegativeBinomial(
#'   Exponential(1), Exponential(1), 10
#' )
NegativeBinomial <- function(alpha = NULL, beta = NULL, bounds = list(NULL, NULL),
                             trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity("NegativeBinomial", bounds, trunc, k, r, param, alpha = alpha, beta = beta)
}

#' @inherit freeParameters
freeParameters.NegativeBinomial <- function(x) {
  alphaStr <- if (is.Density(x$alpha)) {
    alphaBoundsStr <- make_bounds(x, "alpha")
    sprintf(
      "real%s alpha%s%s;",
      alphaBoundsStr, x$k, x$r
    )
  } else {
    ""
  }

  betaStr <- if (is.Density(x$beta)) {
    betaBoundsStr <- make_bounds(x, "beta")
    sprintf(
      "real%s beta%s%s;",
      betaBoundsStr, x$k, x$r
    )
  } else {
    ""
  }

  collapse(alphaStr, betaStr)
}

#' @inherit fixedParameters
fixedParameters.NegativeBinomial <- function(x) {
  alphaStr <-
    if (is.Density(x$alpha)) {
      ""
    } else {
      if (!check_scalar(x$alpha)) {
        stop("If fixed, alpha must be a scalar.")
      }

      sprintf(
        "real alpha%s%s = %s;",
        x$k, x$r, x$alpha
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
        x$k, x$r, x$beta
      )
    }

  collapse(alphaStr, betaStr)
}

#' @inherit generated
generated.NegativeBinomial <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = neg_binomial_rng(alpha%s%s, beta%s%s);",
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

#' @inherit getParameterNames
getParameterNames.NegativeBinomial <- function(x) {
  return(c("alpha", "beta"))
}

#' @inherit logLike
logLike.NegativeBinomial <- function(x) {
  sprintf(
    "loglike[%s][t] = neg_binomial_lpmf(y[t] | alpha%s%s, beta%s%s);",
    x$k,
    x$k, x$r,
    x$k, x$r
  )
}

#' @inherit prior
prior.NegativeBinomial <- function(x) {
  sprintf(
    "%s%s%s ~ neg_binomial(%s, %s);",
    x$param, x$k, x$r,
    x$alpha, x$beta
  )
}
