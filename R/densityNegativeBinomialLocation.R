NegativeBinomialLocation <- function(mu = NULL, phi = NULL, bounds = list(NULL, NULL),
                                     trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  DiscreteDensity(
    "NegativeBinomialLocation",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

freeParameters.NegativeBinomialLocation <- function(x) {
  muStr <- if (is.Density(x$mu)) {
    muBoundsStr <- make_bounds(x, "mu")
    sprintf(
      "real%s mu%s%s;",
      muBoundsStr, x$k, x$r
    )
  } else {
    ""
  }

  phiStr <- if (is.Density(x$phi)) {
    phiBoundsStr <- make_bounds(x, "phi")
    sprintf(
      "real%s phi%s%s;",
      phiBoundsStr, x$k, x$r
    )
  } else {
    ""
  }

  collapse(muStr, phiStr)
}

fixedParameters.NegativeBinomialLocation <- function(x) {
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

  phiStr <-
    if (is.Density(x$phi)) {
      ""
    } else {
      if (!check_scalar(x$phi)) {
        stop("If fixed, phi must be a scalar.")
      }

      sprintf(
        "real NegativeBinomialLocation%s%s = %s;",
        x$k, x$r, x$phi
      )
    }

  collapse(muStr, phiStr)
}

generated.NegativeBinomialLocation <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t][%s] = neg_binomial_2_rng(mu%s%s, phi%s%s);",
    x$k, x$r,
    x$k, x$r,
    x$k, x$r
  )
}

getParameterNames.NegativeBinomialLocation <- function(x) {
  return(c("mu", "phi"))
}

logLike.NegativeBinomialLocation <- function(x) {
  sprintf(
    "loglike[%s][t] = neg_binomial_2_lpmf(y[t] | mu%s%s, phi%s%s);",
    x$k,
    x$k, x$r,
    x$k, x$r
  )
}

prior.NegativeBinomialLocation <- function(x) {
  sprintf(
    "%s%s%s ~ neg_binomial_2(%s, %s);",
    x$param, x$k, x$r,
    x$mu, x$phi
  )
}
