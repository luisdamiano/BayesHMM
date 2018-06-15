RegBinomialLogit <- function(xBeta = NULL, M = NULL, N = NULL, bounds = list(NULL, NULL),
                              trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity(
    "RegBinomialLogit",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

constants.RegBinomialLogit <- function(x) {
  sprintf(
    "int<lower = 1> N = %s; // number of trials",
    x$N
  )
}

data.RegBinomialLogit <- function(x) {
  collapse(
    c(
      "int<lower = 1> M; // number of predictors",
      "matrix[T, M] x;   // predictors",
      NextMethod()
    )
  )
}

freeParameters.RegBinomialLogit <- function(x) {
  xBetaStr <-
    if (is.Density(x$xBeta)) {
      xBetaBoundsStr <- make_bounds(x, "xBeta")
      sprintf(
        "vector%s[M] xBeta%s%s;",
        xBetaBoundsStr, x$k, x$r
      )
    } else {
      ""
    }

  xBetaStr
}

fixedParameters.RegBinomialLogit <- function(x) {
  xBetaStr <-
    if (is.Density(x$xBeta)) {
      ""
    } else {
      if (!check_vector(x$xBeta)) {
        stop("If fixed, xBeta must be a vector.")
      }

      sprintf(
        "vector[M] xBeta%s%s = %s;",
        x$k, x$r, x$xBeta
      )
    }

  xBetaStr
}

generated.RegBinomialLogit <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = binomial_rng(N, logit(x[t] * xBeta%s%s));",
    x$k, x$r,
    x$k, x$r
  )
}

getParameterNames.RegBinomialLogit <- function(x) {
  return(c("xBeta"))
}

logLike.RegBinomialLogit <- function(x) {
  sprintf(
    "loglike[%s][t] = binomial_logit_lpmf(y[t] | N, x[t] * xBeta%s%s);",
    x$k,
    x$k, x$r
  )
}

prior.RegBinomialLogit <- function(x) {
  stop("Not to be used as a prior :)")
}
