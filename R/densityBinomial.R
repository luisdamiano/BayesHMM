Binomial <- function(N = NULL, theta = NULL, bounds = list(NULL, NULL),
                     trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity(
    "Binomial",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.Binomial <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = binomial_rng(N%s%s, theta%s%s);",
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

getParameters.Binomial <- function(x) {
  return(
    list(
      theta = eval(x$theta),
      N = eval(x$N)
    )
  )
}

logLike.Binomial <- function(x) {
  sprintf(
    "loglike[%s][t] = binomial_lpmf(y[t] | N%s%s, theta%s%s);",
    x$k,
    x$k, x$r,
    x$k, x$r
  )
}

parameters.Binomial <- function(x) {
  NBoundsStr        <- make_bounds(x, "N")
  thetaBoundsStr    <- make_bounds(x, "theta")

  sprintf(
    "real%s N%s%s;\nreal%s theta%s%s;",
    NBoundsStr, x$k, x$r,
    thetaBoundsStr, x$k, x$r
  )
}

prior.Binomial <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ binomial(%s, %s) %s;",
    x$param,
    x$k, rStr,
    x$N, x$theta,
    truncStr
  )
}
