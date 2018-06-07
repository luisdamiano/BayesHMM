Dirichlet <- function(alpha = NULL, bounds = list(NULL, NULL),
                      trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "Dirichlet",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.Dirichlet <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

getParameters.Dirichlet <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

is.multivariate.Dirichlet <- function(x) { TRUE }

logLike.Dirichlet <- function(x) {
  stop("You shouldn't be calling this")
}

parameters.Dirichlet <- function(x) {
  stop("You shouldn't be calling this")
}

prior.Dirichlet <- function(x) {
  sprintf("%s%s%s ~ dirichlet(%s);", x$param, x$k, x$r,
          sprintf("[%s]'", paste(eval(x$alpha), collapse = ", ")))
}
