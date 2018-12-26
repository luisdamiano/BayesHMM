#' Fixed transition matrix
#'
#' @inherit Density
#' @param A A numeric matrix of size \emph{KxK} with rows suming to one.
#' @family Density
#' @examples
#' TransitionFixed(
#'   A = matrix(c(0.2, 0.8, 0.9, 0.1), 2, 2)
#' )
TransitionFixed   <- function(A = NULL, bounds = list(NULL, NULL),
                              trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("TransitionFixed", bounds, trunc, k, r, param, A = A)
}

#' @inherit link
link.TransitionFixed <- function(x) {
  if (!(check_transition_matrix(x$A) || dim(x$A)[1] != x$K) ) {
    stop("If fixed, A must be a square matrix of size KxK with simplex rows.")
  }

  sprintf(
    "A[t, i] = %s[i]';",
    matrix_to_stan(x$A)
  )
}
