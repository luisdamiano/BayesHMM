LKJCor <- function(eta = NULL, bounds = list(NULL, NULL),
                   trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  PriorOnlyMultivariateDensity(
    "LKJCor",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
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
