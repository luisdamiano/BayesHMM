NegativeBinomialLocation <- function(mu = NULL, phi = NULL, bounds = list(NULL, NULL),
                                     trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity(
    "NegativeBinomialLocation",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.NegativeBinomialLocation <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = neg_binomial_2_rng(mu%s%s, phi%s%s);",
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

getParameterNames.NegativeBinomialLocation <- function(x) {
  return(c("mu", "phi"))
}

logLike.NegativeBinomialLocation <- function(x) {
  sprintf(
    "loglike[%s][t] = neg_binomial_2_lpmf(y[t] | mu%s%s, phi%s%s);",
    x$k,
    x$k, x$r,
    x$k, x$r
  )
}

freeParameters.NegativeBinomialLocation <- function(x) {
  muBoundsStr <- make_bounds(x, "mu")
  phiBoundsStr  <- make_bounds(x, "phi")

  sprintf(
    "real%s mu%s%s;\nreal%s phi%s%s;",
    muBoundsStr, x$k, x$r,
    phiBoundsStr, x$k, x$r
  )
}

prior.NegativeBinomialLocation <- function(x) {
  sprintf(
    "%s%s%s ~ neg_binomial_2(%s, %s);",
    x$param, x$k, x$r,
    x$mu, x$phi
  )
}
