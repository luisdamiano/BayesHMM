Bernoulli <- function(theta = NULL, bounds = list(NULL, NULL),
                      trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity(
    "Bernoulli",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.Bernoulli <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = bernoulli_rng(theta%s%s);",
    x$k, x$r,
    x$k, x$r
  )
}

getParameters.Bernoulli <- function(x) {
  return(
    list(
      theta = eval(x$theta)
    )
  )
}

logLike.Bernoulli <- function(x) {
  sprintf(
    "loglike[%s][t] = bernoulli_lpmf(y[t] | theta%s%s);",
    x$k,
    x$k, x$r
  )
}

parameters.Bernoulli <- function(x) {
  thetaBoundsStr    <- make_bounds(x, "theta")

  sprintf(
    "real%s theta%s%s;",
    thetaBoundsStr, x$k, x$r
  )
}

prior.Bernoulli <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ bernoulli(%s) %s;",
    x$param,
    x$k, rStr,
    x$theta,
    truncStr
  )
}
