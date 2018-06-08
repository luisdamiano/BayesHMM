MVGaussian <- function(mu = NULL, sigma  = NULL, bounds = list(NULL, NULL),
                       trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "MVGaussian",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.MVGaussian <- function(x) {
  sprintf("if(zpred[t] == %s) ypred[t] = multi_normal_rng(mu%s, sigma%s)';", x$k, x$k, x$k)
}

getParameters.MVGaussian <- function(x) {
  return(list(mu = eval(x$mu), sigma = eval(x$sigma)))
}

is.multivariate.MVGaussian <- function(x) { TRUE }

logLike.MVGaussian <- function(x) {
  # subindStr <- make_subindex(x)
  sprintf("loglike[%s][t] = multi_normal_lpdf(y[t] | mu%s, sigma%s);", x$k, x$k, x$k)
}

parameters.MVGaussian <- function(x) {
  sprintf(
    "vector[%s] mu%s;\ncov_matrix[%s] sigma%s;",
    x$mu$R, x$k,
    x$mu$R, x$k
  )
}

prior.MVGaussian <- function(x) {
  stop("TO BE IMPLEMENTED.")
}
