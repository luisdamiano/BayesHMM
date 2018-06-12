Fixed <- function(value = NULL, bounds = list(NULL, NULL),
                  trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "Fixed",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.Fixed <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

getFreeParameters.Fixed <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

logLike.Fixed <- function(x) {
  stop("You shouldn't be calling this")
}

freeParameters.Fixed <- function(x) {
  stop("You shouldn't be calling this")
}

prior.Fixed <- function(x) {
  sprintf("%s%s%s ~ normal(%s, 0.00000001);", x$param, x$k, x$r, x$value)
}
