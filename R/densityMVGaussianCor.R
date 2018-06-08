MVGaussianCor <- function(mu = NULL, L  = NULL, bounds = list(NULL, NULL),
                          trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "MVGaussianCor",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.MVGaussianCor <- function(x) {
  sprintf("if(zpred[t] == %s) ypred[t] = multi_normal_cholesky_rng(mu%s, L%s)';", x$k, x$k, x$k)
}

getParameters.MVGaussianCor <- function(x) {
  return(list(mu = eval(x$mu), L = eval(x$L)))
}

is.multivariate.MVGaussianCor <- function(x) { TRUE }

logLike.MVGaussianCor <- function(x) {
  # subindStr <- make_subindex(x)
  sprintf("loglike[%s][t] = multi_normal_cholesky_lpdf(y[t] | mu%s, L%s);", x$k, x$k, x$k)
}

parameters.MVGaussianCor <- function(x) {
  sprintf(
    "vector[%s] mu%s;\ncholesky_factor_corr[%s] L%s;",
    x$mu$R, x$k,
    x$mu$R, x$k
  )
}

prior.MVGaussianCor <- function(x) {
  stop("TO BE IMPLEMENTED.")
}
