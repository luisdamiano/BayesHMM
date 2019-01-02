#' Negative Binomial mass in the mean value parametrization (univariate, discrete, bounded space)
#'
#' @inherit Density
#' @param mu  Either a fixed value or a prior density for the mean parameter.
#' @param phi Either a fixed value or a prior density for the dispersion parameter.
#'
#' @family Density
#'
#' @examples
#' # With fixed values for the parameters
#' NegativeBinomialLocation(0.5, 1)
#'
#' # With priors for the parameters
#' NegativeBinomialLocation(
#'   Beta(0, 1), Exponential(1)
#' )
NegativeBinomialLocation <- function(mu = NULL, phi = NULL, ordered = NULL, equal = NULL, bounds = list(NULL, NULL),
                                     trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity("NegativeBinomialLocation", ordered, equal, bounds, trunc, k, r, param, mu = mu, phi = phi)
}

#' @keywords internal
#' @inherit freeParameters
freeParameters.NegativeBinomialLocation <- function(x) {
  muStr <- if (is.Density(x$mu)) {
    muBoundsStr <- make_bounds(x, "mu")
    sprintf(
      "real%s mu%s%s;",
      muBoundsStr, get_k(x, "mu"), get_r(x, "mu")
    )
  } else {
    ""
  }

  phiStr <- if (is.Density(x$phi)) {
    phiBoundsStr <- make_bounds(x, "phi")
    sprintf(
      "real%s phi%s%s;",
      phiBoundsStr, get_k(x, "phi"), get_r(x, "phi")
    )
  } else {
    ""
  }

  collapse(muStr, phiStr)
}

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.NegativeBinomialLocation <- function(x) {
  muStr <-
    if (is.Density(x$mu)) {
      ""
    } else {
      if (!check_scalar(x$mu)) {
        stop("If fixed, mu must be a scalar.")
      }

      sprintf(
        "real mu%s%s = %s;",
        get_k(x, "mu"), get_r(x, "mu"), x$mu
      )
    }

  phiStr <-
    if (is.Density(x$phi)) {
      ""
    } else {
      if (!check_scalar(x$phi)) {
        stop("If fixed, phi must be a scalar.")
      }

      sprintf(
        "real phi%s%s = %s;",
        get_k(x, "phi"), get_r(x, "phi"), x$phi
      )
    }

  collapse(muStr, phiStr)
}

#' @keywords internal
#' @inherit generated
generated.NegativeBinomialLocation <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = neg_binomial_2_rng(mu%s%s, phi%s%s);",
    x$k, x$r,
    get_k(x, "mu"), get_r(x, "mu"),
    get_k(x, "phi"), get_r(x, "phi")
  )
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.NegativeBinomialLocation <- function(x) {
  return(c("mu", "phi"))
}

#' @keywords internal
#' @inherit logLike
logLike.NegativeBinomialLocation <- function(x) {
  sprintf(
    "loglike[%s][t] = neg_binomial_2_lpmf(y[t] | mu%s%s, phi%s%s);",
    x$k,
    get_k(x, "mu"), get_r(x, "mu"),
    get_k(x, "phi"), get_r(x, "phi")
  )
}

#' @keywords internal
#' @inherit prior
prior.NegativeBinomialLocation <- function(x) {
  sprintf(
    "%s%s%s ~ neg_binomial_2(%s, %s);",
    x$param, x$k, x$r,
    x$mu, x$phi
  )
}
