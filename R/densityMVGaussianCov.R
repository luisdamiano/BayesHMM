MVGaussianCov <- function(mu = NULL, L  = NULL, bounds = list(NULL, NULL),
                          trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity(
    "MVGaussianCov",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.MVGaussianCov <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t] = multi_normal_cholesky_rng(mu%s, L%s)';",
    x$k, x$k, x$k
  )
}

getParameterNames.MVGaussianCov <- function(x) {
  return(c("mu", "L"))
}


logLike.MVGaussianCov <- function(x) {
  sprintf(
    "loglike[%s][t] = multi_normal_cholesky_lpdf(y[t] | mu%s, L%s);",
    x$k, x$k, x$k
  )
}

freeParameters.MVGaussianCov <- function(x) {
  sprintf(
    "vector[R] mu%s;\ncholesky_factor_cov[R] L%s;",
    x$k, x$k
  )
}

prior.MVGaussianCov <- function(x) {
  stop("TO BE IMPLEMENTED.")
}
