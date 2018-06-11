MVGaussianCor <- function(mu = NULL, L  = NULL, bounds = list(NULL, NULL),
                          trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity(
    "MVGaussianCor",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.MVGaussianCor <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t] = multi_normal_cholesky_rng(mu%s, L%s)';",
    x$k, x$k, x$k
  )
}

getParameterNames.MVGaussianCor <- function(x) {
  return(c("mu", "L"))
}

logLike.MVGaussianCor <- function(x) {
  sprintf(
    "loglike[%s][t] = multi_normal_cholesky_lpdf(y[t] | mu%s, L%s);",
    x$k, x$k, x$k
  )
}

parameters.MVGaussianCor <- function(x) {
  sprintf(
    "vector[R] mu%s;\ncholesky_factor_corr[R] L%s;",
    x$k, x$k
  )
}

prior.MVGaussianCor <- function(x) {
  stop("TO BE IMPLEMENTED.")
}
