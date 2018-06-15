RegCategoricalSoftmax <- function(xBeta = NULL, M = NULL, N = NULL, bounds = list(NULL, NULL),
                             trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity(
    "RegCategoricalSoftmax",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

constants.RegCategoricalSoftmax <- function(x) {
  sprintf(
    "int<lower = 1> N = %s; // number of categories",
    x$N
  )
}

data.RegCategoricalSoftmax <- function(x) {
  collapse(
    c(
      "int<lower = 1> M; // number of predictors",
      "matrix[T, M] x;   // predictors",
      NextMethod()
    )
  )
}

freeParameters.RegCategoricalSoftmax <- function(x) {
  xBetaStr <-
    if (is.Density(x$xBeta)) {
      # xBetaBoundsStr <- make_bounds(x, "xBeta")
      # sprintf(
      #   "vector%s[M] xBeta%s%s;",
      #   xBetaBoundsStr, x$k, x$r
      # )
      xBetaBoundsStr <- make_bounds(x, "xBeta")
      sprintf(
        "matrix%s[N, M] xBeta%s%s;",
        xBetaBoundsStr, x$k, x$r
      )
    } else {
      ""
    }

  xBetaStr
}

fixedParameters.RegCategoricalSoftmax <- function(x) {
  xBetaStr <-
    if (is.Density(x$xBeta)) {
      ""
    } else {
      if (!check_matrix(x$xBeta)) {
        stop("If fixed, xBeta must be a matrix.")
      }

      sprintf(
        "matrix[N, M] xBeta%s%s = %s;",
        x$k, x$r, x$xBeta
      )
    }

  xBetaStr
}

generated.RegCategoricalSoftmax <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = categorical_logit_rng((x[t] * xBeta%s%s')');",
    x$k, x$r,
    x$k, x$r
  )
}

getParameterNames.RegCategoricalSoftmax <- function(x) {
  return(c("xBeta"))
}

logLike.RegCategoricalSoftmax <- function(x) {
  sprintf(
    "loglike[%s][t] = categorical_logit_lpmf(y[t] | (x[t] * xBeta%s%s')');",
    x$k,
    x$k, x$r
  )
}

prior.RegCategoricalSoftmax <- function(x) {
  stop("Not to be used as a prior :)")
}
