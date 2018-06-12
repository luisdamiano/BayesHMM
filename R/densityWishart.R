Wishart <- function(nu = NULL, sigma = NULL, bounds = list(NULL, NULL),
                    trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity(
    "Wishart",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.Wishart <- function(x) {
  stop("Wishart can only be used as a prior density.")
}

getFreeParameters.Wishart <- function(x) {
  stop("Wishart can only be used as a prior density.")
}

logLike.Wishart <- function(x) {
  stop("Wishart can only be used as a prior density.")
}

freeParameters.Wishart <- function(x) {
  stop("Wishart can only be used as a prior density.")
}

prior.Wishart <- function(x) {
  truncStr <- make_trunc(x, "")
  sprintf("%s%s%s ~ wishart(%s, %s) %s;", x$param, x$k, x$r, x$nu, x$sigma, truncStr)
}
