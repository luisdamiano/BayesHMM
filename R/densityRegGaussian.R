RegGaussian <- function(sigma = NULL, xBeta = NULL, M = NULL, bounds = list(NULL, NULL),
                     trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "RegGaussian",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

data.RegGaussian <- function(x, noLogLike) {
  collapse(
    c(
      "int<lower = 1> M; // number of predictors",
      "matrix[T, M] x;   // predictors",
      NextMethod()
    )
  )
}

freeParameters.RegGaussian <- function(x) {
  xBetaStr <-
    if (is.Density(x$xBeta)) {
      xBetaBoundsStr <- make_bounds(x, "xBeta")
      sprintf(
        "vector%s[M] xBeta%s%s;",
        xBetaBoundsStr, x$k, x$r
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

  collapse(xBetaStr, sigmaStr)
}

fixedParameters.RegGaussian <- function(x) {
  xBetaStr <-
    if (is.Density(x$xBeta)) {
      ""
    } else {
      if (!check_vector(x$xBeta)) {
        stop("If fixed, xBeta must be a vector.")
      }

      sprintf(
        "vector[M] xBeta%s%s = %s;",
        x$k, x$r, x$xBeta
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

  collapse(xBetaStr, sigmaStr)
}

generated.RegGaussian <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = normal_rng(x[t] * xBeta%s%s, sigma%s%s);",
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

getParameterNames.RegGaussian <- function(x) {
  return(c("xBeta", "sigma"))
}

logLike.RegGaussian <- function(x) {
  sprintf(
    "loglike[%s][t] = normal_lpdf(y[t] | x[t] * xBeta%s%s, sigma%s%s);",
    x$k,
    x$k, x$r,
    x$k, x$r
  )
}

prior.RegGaussian <- function(x) {
  stop("Not to be used as a prior :)")
}
