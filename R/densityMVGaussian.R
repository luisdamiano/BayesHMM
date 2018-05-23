MVGaussian <- function(mu = NULL, sigma  = NULL, bounds = list(NULL, NULL),
                       trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "MVGaussian",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.MVGaussian <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

getParameters.MVGaussian <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

is.multivariate.MVGaussian <- function(x) { TRUE }

loglike.MVGaussian <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

parameters.MVGaussian <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

prior.MVGaussian <- function(x) {
  stop("TO BE IMPLEMENTED.")
}
