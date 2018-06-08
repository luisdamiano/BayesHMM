MVStudent <- function(mu = NULL, sigma  = NULL, nu = NULL, bounds = list(NULL, NULL),
                       trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "MVStudent",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.MVStudent <- function(x) {
  sprintf("if(zpred[t] == %s) ypred[t] = multi_student_t_rng(nu%s, mu%s, sigma%s)';", x$k, x$k, x$k, x$k)
}

getParameters.MVStudent <- function(x) {
  return(list(mu = eval(x$mu), sigma = eval(x$sigma), nu = eval(x$nu)))
}

is.multivariate.MVStudent <- function(x) { TRUE }

logLike.MVStudent <- function(x) {
  # subindStr <- make_subindex(x)
  sprintf("loglike[%s][t] = multi_student_t_lpdf(y[t] | nu%s, mu%s, sigma%s);", x$k, x$k, x$k, x$k)
}

parameters.MVStudent <- function(x) {
  nuBoundsStr    <- make_bounds(x, "nu")

  sprintf(
    "vector[%s] mu%s;\ncov_matrix[%s] sigma%s;\nreal%s nu%s;",
    x$mu$R, x$k,
    x$mu$R, x$k,
    nuBoundsStr, x$k
  )
}

prior.MVStudent <- function(x) {
  stop("TO BE IMPLEMENTED.")
}
