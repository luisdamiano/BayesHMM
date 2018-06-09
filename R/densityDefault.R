Default <- function(bounds = list(NULL, NULL),
                    trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "Default",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.Default <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

getParameters.Default <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

logLike.Default <- function(x) {
  stop("You shouldn't be calling this")
}

parameters.Default <- function(x) {
  stop("You shouldn't be calling this")
}

prior.Default <- function(x) {
  sprintf("// %s%s%s ~ Default Stan priors;", x$param, x$k, x$r)
}
