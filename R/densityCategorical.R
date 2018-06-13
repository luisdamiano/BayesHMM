Categorical <- function(theta = NULL, N = NULL, bounds = list(NULL, NULL),
                        trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity(
    "Categorical",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

constants.Categorical <- function(x) {
  sprintf(
    "int<lower = 1> N = %s; // number of possible outcomes (discrete categories)",
    x$N
  )
}

freeParameters.Categorical <- function(x) {
  thetaStr <-
    if (is.Density(x$theta)) {
      sprintf(
        "simplex[N] theta%s%s;",
        x$k, x$r
      )
    } else {
      ""
    }

  thetaStr
}

fixedParameters.Categorical <- function(x) {
  thetaStr <-
    if (is.Density(x$theta)) {
      ""
    } else {
      if (!check_simplex(x$theta)) {
        stop("If fixed, theta must be an unit simplex.")
      }

      sprintf(
        "simplex[N] theta%s%s = %s;",
        x$k, x$r, x$theta
      )
    }

  thetaStr
}

generated.Categorical <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = categorical_rng(theta%s%s);",
    x$k, x$r,
    x$k, x$r
  )
}

getParameterNames.Categorical <- function(x) {
  return("theta")
}

logLike.Categorical <- function(x) {
  sprintf(
    "loglike[%s][t] = categorical_lpmf(y[t] | theta%s%s);",
    x$k,
    x$k, x$r
  )
}

prior.Categorical <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ categorical(%s) %s;",
    x$param,
    x$k, rStr,
    x$theta,
    truncStr
  )
}
