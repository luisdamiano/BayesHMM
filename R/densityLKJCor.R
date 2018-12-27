#' Cholesky LKJ Correlation density (multivariate, continuous, bounded space, prior only)
#'
#' @inherit Density
#' @param eta   Either a fixed value or a prior density for the shape scalar parameter.
#'
#' @family Density
#'
#' @examples
#' # As a prior for the correlation matrix
#' CholeskyLKJCor(1)
CholeskyLKJCor <- function(eta = NULL, bounds = list(NULL, NULL),
                           trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  PriorOnlyMultivariateDensity("CholeskyLKJCor", bounds, trunc, k, r, param, eta = eta)
}

#' @keywords internal
#' @inherit prior
prior.CholeskyLKJCor <- function(x) {
  truncStr <- make_trunc(x, "")
  sprintf(
    "%s%s ~ lkj_corr_cholesky(%s) %s;",
    x$param,
    x$k, x$eta,
    truncStr
  )
}
