#' Student density (univariate, continuous, unbounded space)
#'
#' @inherit Density
#' @param mu    Either a fixed value or a prior density for the location parameter.
#' @param sigma Either a fixed value or a prior density for the shape parameter.
#' @param nu    Either a fixed value or a prior density for the degree-of-freedom parameter.
#'
#' @family Density
#' #'
#'
#' @examples
#' # With fixed values for the parameters
#' Student(0, 1, 1)
#'
#' # With priors for the parameters
#' Student(
#'   mu    = 0,
#'   sigma = Cauchy(mu = 0, sigma = 10, bounds = list(0, NULL)),
#'   nu    = GammaDensity(2, 0.1)
#' )
Student <- function(mu = NULL, sigma  = NULL, nu = NULL,
                    bounds = list(NULL, NULL), trunc  = list(NULL, NULL),
                    k = NULL, r = NULL, param = NULL) {
  Density("Student", bounds, trunc, k, r, param, mu = mu, sigma = sigma, nu = nu)
}

#' @inherit freeParameters
freeParameters.Student <- function(x) {
  muStr <- if (is.Density(x$mu)) {
    muBoundsStr <- make_bounds(x, "mu")
    sprintf(
      "real%s mu%s%s;",
      muBoundsStr, x$k, x$r
    )
  } else {
    ""
  }

  sigmaStr <- if (is.Density(x$sigma)) {
    sigmaBoundsStr <- make_bounds(x, "sigma")
    sprintf(
      "real%s sigma%s%s;",
      sigmaBoundsStr, x$k, x$r
    )
  } else {
    ""
  }

  nuStr <- if (is.Density(x$nu)) {
    nuBoundsStr <- make_bounds(x, "nu")
    sprintf(
      "real%s nu%s%s;",
      nuBoundsStr, x$k, x$r
    )
  } else {
    ""
  }

  collapse(muStr, sigmaStr, nuStr)
}

#' @inherit fixedParameters
fixedParameters.Student <- function(x) {
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

  nuStr <-
    if (is.Density(x$nu)) {
      ""
    } else {
      if (!check_scalar(x$nu)) {
        stop("If fixed, nu must be a scalar.")
      }

      sprintf(
        "real nu%s%s = %s;",
        x$k, x$r, x$nu
      )
    }

  collapse(muStr, sigmaStr, nuStr)
}

#' @inherit generated
generated.Student <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = student_t_rng(nu%s%s, mu%s%s, sigma%s%s);",
    x$k, x$r,
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

#' @inherit getParameterNames
getParameterNames.Student <- function(x) {
  return(c("mu", "sigma", "nu"))
}

#' @inherit logLike
logLike.Student <- function(x) {
  sprintf(
    "loglike[%s][t] = student_t_lpdf(y[t] | nu%s%s, mu%s%s, sigma%s%s);",
    x$k,
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

#' @inherit prior
prior.Student <- function(x) {
  truncStr    <- make_trunc(x, "")
  rStr        <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ student_t(%s, %s, %s) %s;",
    x$param,
    x$k, rStr,
    x$nu, x$mu, x$sigma,
    truncStr
  )
}
