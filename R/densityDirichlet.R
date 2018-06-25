Dirichlet <- function(alpha = NULL, bounds = list(NULL, NULL),
                      trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity(
    "Dirichlet",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

freeParameters.Dirichlet <- function(x) {
  ""
  # stop("TO BE IMPLEMENTED.")
}

fixedParameters.Dirichlet <- function(x) {
  ""
  # stop("TO BE IMPLEMENTED.")
}

generated.Dirichlet <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

getParameterNames.Dirichlet <- function(x) {
  return(c("alpha"))
}

logLike.Dirichlet <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

prior.Dirichlet <- function(x) {
  sprintf("%s%s%s ~ dirichlet(%s);", x$param, x$k, x$r,
          sprintf("[%s]'", paste(eval(x$alpha), collapse = ", ")))
}
