#' Multivariate Gaussian density with Cholesky decomposition of the correlation matrix (Multivariate, continuous, unbounded space)
#'
#' @inherit Density
#' @param mu    Either a fixed value or a prior density for the mean vector.
#' @param L     Either a fixed value or a prior density for the Cholesky factor of the correlation matrix.
#'
#' @family Density
#'
#' @examples
#' # With priors for the parameters
#' MVGaussianCholeskyCor(
#'   mu    = MVGaussian(mu = c(0, 0), sigma = matrix(c(1, 0, 0, 1), 2, 2)),
#'   L     = CholeskyLKJCor(1)
#' )
MVGaussianCholeskyCor <- function(mu = NULL, L  = NULL, ordered = NULL, bounds = list(NULL, NULL),
                                  trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity("MVGaussianCholeskyCor", ordered, bounds, trunc, k, r, param, mu = mu, L = L)
}

#' @keywords internal
#' @inherit freeParameters
freeParameters.MVGaussianCholeskyCor <- function(x) {
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

#' @keywords internal
#' @inherit fixedParameters
fixedParameters.MVGaussianCholeskyCor <- function(x) {
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

#' @keywords internal
#' @inherit generated
generated.MVGaussianCholeskyCor <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t] = multi_normal_cholesky_rng(mu%s, L%s)';",
    x$k, x$k, x$k
  )
}

#' @keywords internal
#' @inherit getParameterNames
getParameterNames.MVGaussianCholeskyCor <- function(x) {
  return(c("mu", "L"))
}

#' @keywords internal
#' @inherit logLike
logLike.MVGaussianCholeskyCor <- function(x) {
  sprintf(
    "loglike[%s][t] = multi_normal_cholesky_lpdf(y[t] | mu%s, L%s);",
    x$k, x$k, x$k
  )
}

#' @keywords internal
#' @inherit prior
prior.MVGaussianCholeskyCor <- function(x) {
  stop("TO BE IMPLEMENTED.")
}
