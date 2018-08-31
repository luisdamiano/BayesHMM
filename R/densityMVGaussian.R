MVGaussian <- function(mu = NULL, sigma  = NULL, bounds = list(NULL, NULL),
                       trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity(
    "MVGaussian",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

freeParameters.MVGaussian <- function(x) {
  muStr <-
    if (is.Density(x$mu)) {
      sprintf(
        "vector[R] mu%s;",
        x$k
      )
    } else {
      ""
    }

  sigmaStr <-
    if (is.Density(x$sigma)) {
      sprintf(
        "cov_matrix[R] sigma%s;",
        x$k
      )
    } else {
      ""
    }

  collapse(muStr, sigmaStr)
}

fixedParameters.MVGaussian <- function(x) {
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

  sigmaStr <-
    if (is.Density(x$sigma)) {
      ""
    } else {
      if (!check_cov_matrix(x$sigma)) {
        stop("If fixed, sigma must be a matrix of size RxR")
      }

      sprintf(
        "matrix[R, R] sigma%s = %s;",
        x$k, matrix_to_stan(x$sigma)
      )
    }

  collapse(muStr, sigmaStr)
}

generated.MVGaussian <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t] = multi_normal_rng(mu%s, sigma%s)';",
    x$k, x$k, x$k
  )
}

getParameterNames.MVGaussian <- function(x) {
  return(c("mu", "sigma"))
}

logLike.MVGaussian <- function(x) {
  sprintf(
    "loglike[%s][t] = multi_normal_lpdf(y[t] | mu%s, sigma%s);",
    x$k, x$k, x$k
  )
}

prior.MVGaussian <- function(x) {
  truncStr <- make_trunc(x, "")
  sprintf(
    "%s%s ~ multi_normal(%s, %s) %s;",
    x$param, x$k,
    vector_to_stan(x$mu), matrix_to_stan(x$sigma),
    truncStr
  )
}
