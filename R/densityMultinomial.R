Multinomial <- function(theta = NULL, N = NULL, bounds = list(NULL, NULL),
                        trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDiscreteDensity(
    "Multinomial",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

constants.Multinomial <- function(x) {
  sprintf(
    "int<lower = 1> N = %s; // number of trials",
    x$N
  )
}

freeParameters.Multinomial <- function(x) {
  thetaStr <-
    if (is.Density(x$theta)) {
      sprintf(
        "simplex[R] theta%s%s;",
        x$k, x$r
      )
    } else {
      ""
    }

  thetaStr
}

fixedParameters.Multinomial <- function(x) {
  thetaStr <-
    if (is.Density(x$theta)) {
      ""
    } else {
      if (!check_simplex(x$theta)) {
        stop("If fixed, theta must be an unit simplex")
      }

      sprintf(
        "simplex[R theta%s%s = %s;",
        x$k, x$r, x$theta
      )
    }

  thetaStr
}
generated.Multinomial <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t] = to_row_vector(multinomial_rng(theta%s%s, N));",
    x$k,
    x$k, x$r
  )
}

getParameterNames.Multinomial <- function(x) {
  return("theta")
}

logLike.Multinomial <- function(x) {
  sprintf(
    "loglike[%s][t] = multinomial_lpmf(y[t] | theta%s%s);",
    x$k,
    x$k, x$r
  )
}

prior.Multinomial <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ multinomial(%s) %s;",
    x$param,
    x$k, rStr,
    x$theta,
    truncStr
  )
}
