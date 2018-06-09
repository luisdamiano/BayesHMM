Gaussian <- function(mu = NULL, sigma  = NULL, bounds = list(NULL, NULL),
                     trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "Gaussian",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.Gaussian <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = normal_rng(mu%s%s, sigma%s%s);",
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

getParameters.Gaussian <- function(x) {
  return(list(mu = eval(x$mu), sigma = eval(x$sigma)))
}

logLike.Gaussian <- function(x) {
  sprintf(
    "loglike[%s][t] = normal_lpdf(y[t] | mu%s%s, sigma%s%s);",
    x$k,
    x$k, x$r,
    x$k, x$r
  )
}

parameters.Gaussian <- function(x) {
  muBoundsStr    <- make_bounds(x, "mu")
  sigmaBoundsStr <- make_bounds(x, "sigma")

  sprintf(
    "real%s mu%s%s;\nreal%s sigma%s%s;",
    muBoundsStr, x$k, x$r,
    sigmaBoundsStr, x$k, x$r
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
