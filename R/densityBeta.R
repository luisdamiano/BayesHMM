Beta <- function(alpha = NULL, beta = NULL, bounds = list(NULL, NULL),
                 trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "Beta",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.Beta <- function(x) {
  sprintf("if(zpred[t] == %s) ypred[t][%s] = beta_rng(alpha%s%s, beta%s%s);", x$k, x$r, x$k, x$r, x$k, x$r)
}

getParameters.Beta <- function(x) {
  return(list(alpha = eval(x$alpha), beta = eval(x$beta)))
}

logLike.Beta <- function(x) {
  sprintf("loglike[%s][t] = beta_lpdf(y[t] | alpha%s%s, beta%s%s);", x$k, x$k, x$r, x$k, x$r)
}

parameters.Beta <- function(x) {
  alphaBoundsStr <- make_bounds(x, "alpha")
  betaBoundsStr  <- make_bounds(x, "beta")

  sprintf(
    "real%s alpha%s%s;\nreal%s beta%s%s;",
    alphaBoundsStr, x$k, x$r,
    betaBoundsStr, x$k, x$r
  )
}

prior.Beta <- function(x) {
  sprintf("%s%s%s ~ beta(%s, %s);", x$param, x$k, x$r, x$alpha, x$beta)
}
