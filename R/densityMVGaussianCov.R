MVGaussianCov <- function(mu = NULL, L  = NULL, bounds = list(NULL, NULL),
                          trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "MVGaussianCov",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.MVGaussianCov <- function(x) {
  sprintf("if(zpred[t] == %s) ypred[t] = multi_normal_cholesky_rng(mu%s, L%s)';", x$k, x$k, x$k)
}

getParameters.MVGaussianCov <- function(x) {
  return(list(mu = eval(x$mu), L = eval(x$L)))
}

is.multivariate.MVGaussianCov <- function(x) { TRUE }

logLike.MVGaussianCov <- function(x) {
  # subindStr <- make_subindex(x)
  sprintf("loglike[%s][t] = multi_normal_cholesky_lpdf(y[t] | mu%s, L%s);", x$k, x$k, x$k)
}

parameters.MVGaussianCov <- function(x) {
  sprintf(
    "vector[%s] mu%s;\ncholesky_factor_cov[%s] L%s;",
    x$mu$R, x$k,
    x$mu$R, x$k
  )
}

prior.MVGaussianCov <- function(x) {
  stop("TO BE IMPLEMENTED.")
}
