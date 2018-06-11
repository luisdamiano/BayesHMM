MVStudent <- function(mu = NULL, sigma  = NULL, nu = NULL, bounds = list(NULL, NULL),
                       trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity(
    "MVStudent",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.MVStudent <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t] = multi_student_t_rng(nu%s, mu%s, sigma%s)';",
    x$k, x$k, x$k, x$k
  )
}

getParameterNames.MVStudent <- function(x) {
  return(c("mu", "sigma", "nu"))
}

logLike.MVStudent <- function(x) {
  sprintf(
    "loglike[%s][t] = multi_student_t_lpdf(y[t] | nu%s, mu%s, sigma%s);",
    x$k, x$k, x$k, x$k
  )
}

parameters.MVStudent <- function(x) {
  nuBoundsStr    <- make_bounds(x, "nu")
  sprintf(
    "vector[R] mu%s;\ncov_matrix[R] sigma%s;\nreal%s nu%s;",
    x$k, x$k,
    nuBoundsStr, x$k
  )
}

prior.MVStudent <- function(x) {
  stop("TO BE IMPLEMENTED.")
}
