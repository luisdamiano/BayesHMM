Default <- function(bounds = list(NULL, NULL),
                    trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  PriorOnlyDensity(
    "Default",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

prior.Default <- function(x) {
  sprintf("// %s%s%s ~ Default Stan priors;", x$param, x$k, x$r)
}
