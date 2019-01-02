#' Cauchy density (univariate, continuous, unbounded space)
#'
#' @inherit Density
#' @param mu    Either a fixed value or a prior density for the location parameter.
#' @param sigma AEither a fixed value or a prior density for the shape parameter.
#'
#' @family Density
#'
#' @examples
#' # With fixed values for the parameters
#' Cauchy(0, 1)
#'
#' # With priors for the parameters
#' Cauchy(
#'   mu    = Cauchy(mu = 0, sigma = 10),
#'   sigma = Cauchy(mu = 0, sigma = 10, bounds = list(0, NULL))
#' )
Cauchy <- function(mu = NULL, sigma  = NULL, ordered = NULL, equal = NULL, bounds = list(NULL, NULL),
                   trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("Cauchy", ordered, equal, bounds, trunc, k, r, param, mu = mu, sigma = sigma)
}

#' @keywords internal
#' @inherit freeParameters
freeParameters.Cauchy <- function(x) {
  muStr <- if (is.Density(x$mu)) {
    muBoundsStr <- make_bounds(x, "mu")
    sprintf(
      "real%s mu%s%s;",
      muBoundsStr, get_k(x, "mu"), get_r(x, "mu")
    )
  } else {
    ""
  }

  sigmaStr <- if (is.Density(x$sigma)) {
    sigmaBoundsStr <- make_bounds(x, "sigma")
    sprintf(
      "real%s sigma%s%s;",
      sigmaBoundsStr, get_k(x, "sigma"), get_r(x, "sigma")
    )
  } else {
    ""
  }

  collapse(muStr, sigmaStr)
}

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.Cauchy <- function(x) {
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

  sigmaStr <-
    if (is.Density(x$sigma)) {
      ""
    } else {
      if (!check_scalar(x$sigma)) {
        stop("If fixed, sigma must be a scalar.")
      }

      sprintf(
        "real sigma%s%s = %s;",
        get_k(x, "sigma"), get_r(x, "sigma"), x$sigma
      )
    }

  collapse(muStr, sigmaStr)
}

#' @keywords internal
#' @inherit generated
generated.Cauchy <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = cauchy_rng(mu%s%s, sigma%s%s);",
    x$k, x$r,
    get_k(x, "mu"), get_r(x, "mu"),
    get_k(x, "sigma"), get_r(x, "sigma")
  )
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.Cauchy <- function(x) {
  return(c("mu", "sigma"))
}

#' @keywords internal
#' @inherit logLike
logLike.Cauchy <- function(x) {
  sprintf(
    "loglike[%s][t] = cauchy_lpdf(y[t] | mu%s%s, sigma%s%s);",
    x$k,
    get_k(x, "mu"), get_r(x, "mu"),
    get_k(x, "sigma"), get_r(x, "sigma")
  )
}

#' @keywords internal
#' @inherit prior
prior.Cauchy <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ cauchy(%s, %s) %s;",
    x$param,
    x$k, rStr,
    x$mu, x$sigma,
    truncStr
  )
}
