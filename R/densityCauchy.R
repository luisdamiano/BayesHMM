Cauchy <- function(mu = NULL, sigma  = NULL, bounds = list(NULL, NULL),
                   trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density(
    "Cauchy",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

freeParameters.Cauchy <- function(x) {
  muStr <- if (is.Density(x$mu)) {
    muBoundsStr <- make_bounds(x, "mu")
    sprintf(
      "real%s mu%s%s;",
      muBoundsStr, x$k, x$r
    )
  } else {
    ""
  }

  sigmaStr <- if (is.Density(x$sigma)) {
    sigmaBoundsStr <- make_bounds(x, "sigma")
    sprintf(
      "real%s sigma%s%s;",
      sigmaBoundsStr, x$k, x$r
    )
  } else {
    ""
  }

  collapse(muStr, sigmaStr)
}

fixedParameters.Cauchy <- function(x) {
  muStr <-
    if (is.Density(x$mu)) {
      ""
    } else {
      if (!check_scalar(x$mu)) {
        stop("If fixed, mu must be a scalar.")
      }

      sprintf(
        "real mu%s%s = %s;",
        x$k, x$r, x$mu
      )
    }

  sigmaStr <-
    if (is.Density(x$sigma)) {
      ""
    } else {
      if (!check_scalar(x$sigma)) {
        stop("If fixed, sigma must be a scalar.")
      }

      sprintf(
        "real sigma%s%s = %s;",
        x$k, x$r, x$sigma
      )
    }

  collapse(muStr, sigmaStr)
}

generated.Cauchy <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = cauchy_rng(mu%s%s, sigma%s%s);",
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

getParameterNames.Cauchy <- function(x) {
  return(c("mu", "sigma"))
}

logLike.Cauchy <- function(x) {
  sprintf(
    "loglike[%s][t] = cauchy_lpdf(y[t] | mu%s%s, sigma%s%s);",
    x$k,
    x$k, x$r,
    x$k, x$r
  )
}

prior.Cauchy <- function(x) {
  truncStr <- make_trunc(x, "")
  rStr     <- make_rsubindex(x)
  sprintf(
    "%s%s%s ~ cauchy(%s, %s) %s;",
    x$param,
    x$k, rStr,
    x$mu, x$sigma,
    truncStr
  )
}
