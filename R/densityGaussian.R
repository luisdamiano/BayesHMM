#' Gaussian Density (univariate, continuous)
#'
#' @inherit Density
#' @param mu Either a density with a prior for the location parameter or a fixed value.
#' @param sigma Either a density with a prior for the scale parameter or a fixed value.
#' @param bounds (optional) A list with two elements specifying the lower and upper bound for the parameter space. Use either a fixed value for a finite bound or NULL for no bounds. It defaults to an unbounded parameter space.
#' @param trunc (optional) A list with two elements specifying the lower and upper bound for the domain of the density function. Use either a fixed value for a finite bound or NULL for no truncation. It defaults to an unbounded domain.
#' @param k (optional) The number of the hidden state for which this density should be used. This argument is mostly for internal use: you should not use it unless you are acquainted with the internals of this software.
#' @param r (optional) The dimension of the observation vector dimension for which this density should be used. This argument is mostly for internal use: you should not use it unless you are acquainted with the internals of this software.
#' @param param (optional) The name of the parameter. This argument is mostly for internal use: you should not use it unless you are acquainted with the internals of this software.
#'
#' @family Density
#' @export
#'
#' @example
#' # As a Density for the observed variable
#' Gaussian(
#'   mu    = Gaussian(mu = 0, sigma = 10),
#'   sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
#' )
Gaussian <- function(mu = NULL, sigma = NULL, bounds = list(NULL, NULL),
                     trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "Gaussian",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

freeParameters.Gaussian <- function(x) {
  muStr <-
    if (is.Density(x$mu)) {
      muBoundsStr <- make_bounds(x, "mu")
      sprintf(
        "real%s mu%s%s;",
        muBoundsStr, x$k, x$r
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

  collapse(muStr, sigmaStr)
}

fixedParameters.Gaussian <- function(x) {
  muStr <-
    if (is.Density(x$mu)) {
      ""
    } else {
      if (!check_scalar(x$mu)) {
        stop("If fixed, mu must be a scalar.")
      }

      sprintf(
        "real mu%s%s = %s;",
        x$k, x$r, x$mu
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

  collapse(muStr, sigmaStr)
}

generated.Gaussian <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = normal_rng(mu%s%s, sigma%s%s);",
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

getParameterNames.Gaussian <- function(x) {
  return(c("mu", "sigma"))
}

logLike.Gaussian <- function(x) {
  sprintf(
    "loglike[%s][t] = normal_lpdf(y[t] | mu%s%s, sigma%s%s);",
    x$k,
    x$k, x$r,
    x$k, x$r
  )
}

prior.Gaussian <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ normal(%s, %s) %s;",
    x$param,
    x$k, rStr,
    x$mu, x$sigma,
    truncStr
  )
}
