Wishart <- function(nu = NULL, sigma = NULL, bounds = list(NULL, NULL),
                    trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  PriorOnlyMultivariateDensity(
    "Wishart",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

prior.Wishart <- function(x) {
  truncStr <- make_trunc(x, "")
  sprintf("%s%s%s ~ wishart(%s, %s) %s;", x$param, x$k, x$r, x$nu, x$sigma, truncStr)
}
