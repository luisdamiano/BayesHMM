Student <- function(mu = NULL, sigma  = NULL, nu = NULL,
                    bounds = list(NULL, NULL), trunc  = list(NULL, NULL),
                    k = NULL, r = NULL, param = NULL) {
  Density(
    "Student",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.Student <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

getParameters.Student <- function(x) {
  return(list(mu = eval(x$mu), sigma = eval(x$sigma), nu = eval(x$nu)))
}

is.multivariate.Student <- function(x) { FALSE }

loglike.Student <- function(x) {
  subindStr <- make_subindex(x)
  sprintf(
    "loglike%s[t] =  student_t_lpdf(y[t] | nu%s%s, mu%s%s, sigma%s%s);",
    subindStr, x$k, x$r, x$k, x$r, x$k, x$r
  )
}

parameters.Student <- function(x) {
  muBoundsStr    <- make_bounds(x, "mu")
  sigmaBoundsStr <- make_bounds(x, "sigma")
  nuBoundsStr    <- make_bounds(x, "nu")

  sprintf(
    "real%s mu%s%s;\nreal%s sigma%s%s;\nreal%s nu%s%s;",
    muBoundsStr, x$k, x$r,
    sigmaBoundsStr, x$k, x$r,
    nuBoundsStr, x$k, x$r
  )
}

prior.Student <- function(x) {
  truncStr    <- make_trunc(x, "")
  sprintf("%s%s%s ~ student_t(%s, %s, %s) %s;", x$param, x$k, x$r, x$nu, x$mu, x$sigma, truncStr)
}
