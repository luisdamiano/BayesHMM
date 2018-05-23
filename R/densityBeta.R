Beta <- function(alpha = NULL, beta = NULL, bounds = list(NULL, NULL),
                 trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "Beta",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.Beta <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

getParameters.Beta <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

is.multivariate.Beta <- function(x) { FALSE }

loglike.Beta <- function(x) {
  stop("You shouldn't be calling this")
}

parameters.Beta <- function(x) {
  stop("You shouldn't be calling this")
}

prior.Beta <- function(x) {
  sprintf("%s%s%s ~ beta(%s, %s);", x$param, x$k, x$r, x$alpha, x$beta)
}
