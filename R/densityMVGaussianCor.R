MVGaussianCor <- function(mu = NULL, L  = NULL, bounds = list(NULL, NULL),
                          trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity(
    "MVGaussianCor",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

freeParameters.MVGaussianCor <- function(x) {
  muStr <-
    if (is.Density(x$mu)) {
      sprintf(
        "vector[R] mu%s;",
        x$k
      )
    } else {
      ""
    }

  LStr <-
    if (is.Density(x$L)) {
      sprintf(
        "cholesky_factor_corr[R] L%s;",
        x$k
      )
    } else {
      ""
    }

  collapse(muStr, LStr)
}

fixedParameters.MVGaussianCor <- function(x) {
  muStr <-
    if (is.Density(x$mu)) {
      ""
    } else {
      if (!check_vector(x$mu)) {
        stop(sprintf("If fixed, mu must be a vector of size R"))
      }

      sprintf(
        "vector[R] mu%s = %s';",
        x$k, vector_to_stan(x$mu)
      )
    }

  LStr <-
    if (is.Density(x$L)) {
      ""
    } else {
      if (!check_cholesky_factor_cor(x$L)) {
        stop("If fixed, L must be a valid Cholesky factor for a correlation matrix")
      }

      sprintf(
        "cholesky_factor_corr[R] L%s = %s;",
        x$k, matrix_to_stan(x$L)
      )
    }

  collapse(muStr, LStr)
}

generated.MVGaussianCor <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t] = multi_normal_cholesky_rng(mu%s, L%s)';",
    x$k, x$k, x$k
  )
}

getParameterNames.MVGaussianCor <- function(x) {
  return(c("mu", "L"))
}

logLike.MVGaussianCor <- function(x) {
  sprintf(
    "loglike[%s][t] = multi_normal_cholesky_lpdf(y[t] | mu%s, L%s);",
    x$k, x$k, x$k
  )
}

prior.MVGaussianCor <- function(x) {
  stop("TO BE IMPLEMENTED.")
}
