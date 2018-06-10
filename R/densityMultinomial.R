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

generated.Multinomial <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t] = to_row_vector(multinomial_rng(theta%s%s, N));",
    x$k,
    x$k, x$r
  )

  # sprintf("ypred[t][1] = 1;ypred[t][2] = 1;ypred[t][3] = 1;ypred[t][4] = 1;")
}

getParameters.Multinomial <- function(x) {
  return(
    list(
      theta = eval(x$theta)
    )
  )
}

logLike.Multinomial <- function(x) {
  sprintf(
    "loglike[%s][t] = multinomial_lpmf(y[t] | theta%s%s);",
    x$k,
    x$k, x$r
  )
}

parameters.Multinomial <- function(x) {
  thetaBoundsStr    <- make_bounds(x, "theta")

  sprintf(
    "simplex[R]%s theta%s%s;",
    thetaBoundsStr, x$k, x$r
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
