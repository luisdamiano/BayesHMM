Beta <- function(alpha = NULL, beta = NULL, bounds = list(NULL, NULL),
                 trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "Beta",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

freeParameters.Beta <- function(x) {
  alphaStr <-
    if (is.Density(x$alpha)) {
      alphaBoundsStr <- make_bounds(x, "alpha")
      sprintf(
        "real%s alpha%s%s;",
        alphaBoundsStr, x$k, x$r
      )
    } else {
      ""
    }

  betaStr <-
    if (is.Density(x$beta)) {
      betaBoundsStr <- make_bounds(x, "beta")
      sprintf(
        "real%s beta%s%s;",
        betaBoundsStr, x$k, x$r
      )
    } else {
      ""
    }

  collapse(alphaStr, betaStr)
}

fixedParameters.Beta <- function(x) {
  alphaStr <-
    if (is.Density(x$alpha)) {
      ""
    } else {
      if (!check_scalar(x$alpha)) {
        stop("If fixed, alpha must be a scalar.")
      }

      sprintf(
        "real alpha%s%s = %s;",
        x$k, x$r, x$alpha
      )
    }

  betaStr <-
    if (is.Density(x$beta)) {
      ""
    } else {
      if (!check_scalar(x$beta)) {
        stop("If fixed, beta must be a scalar.")
      }

      sprintf(
        "real beta%s%s = %s;",
        x$k, x$r, x$beta
      )
    }

  collapse(alphaStr, betaStr)
}

generated.Beta <- function(x) {
  sprintf("if(zpred[t] == %s) ypred[t][%s] = beta_rng(alpha%s%s, beta%s%s);", x$k, x$r, x$k, x$r, x$k, x$r)
}

getParameterNames.Beta <- function(x) {
  return(c("alpha", "beta"))
}

logLike.Beta <- function(x) {
  sprintf("loglike[%s][t] = beta_lpdf(y[t] | alpha%s%s, beta%s%s);", x$k, x$k, x$r, x$k, x$r)
}

prior.Beta <- function(x) {
  sprintf("%s%s%s ~ beta(%s, %s);", x$param, x$k, x$r, x$alpha, x$beta)
}
