#' Multivariate Gaussian density (Multivariate, continuous, unbounded space)
#'
#' @inherit Density
#' @param mu    Either a fixed value or a prior density for the mean vector.
#' @param sigma Either a fixed value or a prior density for the covariance matrix.
#'
#' @family Density
#' #'
#'
#' @examples
#' # With fixed values for the parameters
#' MVGaussian(
#'   mu    = c(0, 0),
#'   sigma = matrix(c(1, 0, 0, 1), 2, 2)
#' )
#'
#' # With priors for the parameters
#' MVGaussian(
#'   mu    = MVGaussian(mu = c(0, 0), sigma = matrix(c(1, 0, 0, 1), 2, 2)),
#'   sigma = InverseWishart(nu = 5, sigma = matrix(c(1, 0, 0, 1), 2, 2))
#' )
MVGaussian <- function(mu = NULL, sigma  = NULL, bounds = list(NULL, NULL),
                       trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity("MVGaussian", bounds, trunc, k, r, param, mu = mu, sigma = sigma)
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
