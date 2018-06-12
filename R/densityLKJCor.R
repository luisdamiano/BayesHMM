LKJCor <- function(eta = NULL, bounds = list(NULL, NULL),
                   trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity(
    "LKJCor",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.LKJCor <- function(x) {
  stop("LKJCor can only be used as a prior density.")
}

getFreeParameters.LKJCor <- function(x) {
  stop("LKJCor can only be used as a prior density.")
}

logLike.LKJCor <- function(x) {
  stop("LKJCor can only be used as a prior density.")
}

freeParameters.LKJCor <- function(x) {
  stop("LKJCor can only be used as a prior density.")
}

prior.LKJCor <- function(x) {
  truncStr <- make_trunc(x, "")
  sprintf(
    "%s%s ~ lkj_corr_cholesky(%s) %s;",
    x$param,
    x$k, x$eta,
    truncStr
  )
}
