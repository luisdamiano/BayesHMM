RegBernoulliLogit <- function(xBeta = NULL, M = NULL, bounds = list(NULL, NULL),
                        trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity(
    "RegBernoulliLogit",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

data.RegBernoulliLogit <- function(x) {
  collapse(
    c(
      "int<lower = 1> M; // number of predictors",
      "matrix[T, M] x;   // predictors",
      NextMethod()
    )
  )
}

freeParameters.RegBernoulliLogit <- function(x) {
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

fixedParameters.RegBernoulliLogit <- function(x) {
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

generated.RegBernoulliLogit <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = bernoulli_logit_rng(x[t] * xBeta%s%s);",
    x$k, x$r,
    x$k, x$r
  )
}

getParameterNames.RegBernoulliLogit <- function(x) {
  return(c("xBeta"))
}

logLike.RegBernoulliLogit <- function(x) {
  sprintf(
    "loglike[%s][t] = bernoulli_logit_lpmf(y[t] | x[t] * xBeta%s%s);",
    x$k,
    x$k, x$r
  )
}

prior.RegBernoulliLogit <- function(x) {
  stop("Not to be used as a prior :)")
}
