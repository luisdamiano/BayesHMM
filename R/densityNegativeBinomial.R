NegativeBinomial <- function(alpha = NULL, beta = NULL, bounds = list(NULL, NULL),
                             trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity(
    "NegativeBinomial",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.NegativeBinomial <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = neg_binomial_rng(alpha%s%s, beta%s%s);",
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

getParameters.NegativeBinomial <- function(x) {
  return(
    list(
      alpha = eval(x$alpha),
      beta = eval(x$beta)
    )
  )
}

logLike.NegativeBinomial <- function(x) {
  sprintf(
    "loglike[%s][t] = neg_binomial_lpmf(y[t] | alpha%s%s, beta%s%s);",
    x$k,
    x$k, x$r,
    x$k, x$r
  )
}

parameters.NegativeBinomial <- function(x) {
  alphaBoundsStr <- make_bounds(x, "alpha")
  betaBoundsStr  <- make_bounds(x, "beta")

  sprintf(
    "real%s alpha%s%s;\nreal%s beta%s%s;",
    alphaBoundsStr, x$k, x$r,
    betaBoundsStr, x$k, x$r
  )
}

prior.NegativeBinomial <- function(x) {
  sprintf(
    "%s%s%s ~ neg_binomial(%s, %s);",
    x$param, x$k, x$r,
    x$alpha, x$beta
  )
}
