MVGaussian <- function(mu = NULL, sigma  = NULL, bounds = list(NULL, NULL),
                       trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity(
    "MVGaussian",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.MVGaussian <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t] = multi_normal_rng(mu%s, sigma%s)';",
    x$k, x$k, x$k
  )
}

getParameterNames.MVGaussian <- function(x) {
  return(c("mu", "sigma"))
}

logLike.MVGaussian <- function(x) {
  sprintf(
    "loglike[%s][t] = multi_normal_lpdf(y[t] | mu%s, sigma%s);",
    x$k, x$k, x$k
  )
}

parameters.MVGaussian <- function(x) {
  sprintf(
    "vector[R] mu%s;\ncov_matrix[R] sigma%s;",
    x$k, x$k
  )
}

prior.MVGaussian <- function(x) {
  stop("TO BE IMPLEMENTED.")
}
