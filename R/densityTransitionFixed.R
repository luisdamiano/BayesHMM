TransitionFixed   <- function(A = NULL, bounds = list(NULL, NULL),
                              trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  LinkDensity(
    "TransitionFixed",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

block_data.TransitionFixed <- function(x, noLogLike) {
  ""
}

freeParameters.TransitionFixed <- function(x) {
  ""
}

fixedParameters.TransitionFixed <- function(x) {
  ""
}

generated.TransitionFixed <- function(x) {
  ""
}

getParameterNames.TransitionFixed <- function(x) {
  ""
}

logLike.TransitionFixed <- function(x) {
  ""
}

link.TransitionFixed <- function(x) {
  if (!(check_transition_matrix(x$A) || dim(x$A)[1] != x$K) ) {
    stop("If fixed, A must be a square matrix of size KxK with simplex rows.")
  }

  sprintf(
    "A[t, i] = %s[i]';",
    matrix_to_stan(x$A)
  )

  # sprintf(
  #   "matrix[K, K] tmp = %s; A[t, i] = tmp[i]';",
  #   matrix_to_stan(x$A)
  # )

  # sprintf(
  #   "matrix[K, K] A = %s;",
  #   matrix_to_stan(x$A)
  # )
}

prior.TransitionFixed <- function(x) {
  ""
}
