Student <- function(mu = NULL, sigma  = NULL, nu = NULL,
                    bounds = list(NULL, NULL), trunc  = list(NULL, NULL),
                    k = NULL, r = NULL, param = NULL) {
  Density(
    "Student",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

generated.Student <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = student_t_rng(nu%s%s, mu%s%s, sigma%s%s);",
    x$k, x$r,
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

getParameterNames.Student <- function(x) {
  return(c("mu", "sigma", "nu"))
}

logLike.Student <- function(x) {
  sprintf(
    "loglike[%s][t] = student_t_lpdf(y[t] | nu%s%s, mu%s%s, sigma%s%s);",
    x$k,
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

freeParameters.Student <- function(x) {
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
  # rStr        <- sprintf(if (x$multivariate) { "[%s]" } else { "%s" }, x$r)
  rStr        <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ student_t(%s, %s, %s) %s;",
    x$param,
    x$k, rStr,
    x$nu, x$mu, x$sigma,
    truncStr
  )
}
