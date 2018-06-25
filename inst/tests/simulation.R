# add unit tests for rdirichlet and all the other stuff
rdirichlet <- function(alpha) {
  y <- sapply(alpha, function(a) {rgamma(n = 1, shape = a)})
  return(y / sum(y))
  # extraDistr::rdirichlet(1, alpha)
}

sim_hmm_T <- function(lambda) {
  rpois(n = 1, lambda = lambda)
}

sim_hmm_K <- function(lambda) {
  rpois(n = 1, lambda = lambda)
}

sim_hmm_A <- function(alpha) {
  mat <-
    if (is.matrix(alpha)) {
      apply(alpha, 2, function(x) {
        rdirichlet(x)
      })
    } else {
      sapply(1:NROW(alpha), function(x) {
        rdirichlet(alpha)
      })
    }

  t(mat)
}

sim_hmm_pi <- function(alpha) {
  rdirichlet(alpha)
}

sim_hmm_parameters <- function(TLambda = 300, KLambda = 3) {
  T       <- sim_hmm_T(TLambda)
  K       <- sim_hmm_K(KLambda)
  AAlpha  <- abs(rnorm(n = K, mean = 0, sd = 1))
  piAlpha <- abs(rnorm(n = K, mean = 0, sd = 1))
  A       <- sim_hmm_A(AAlpha)
  pi      <- sim_hmm_pi(piAlpha)
  list(T = T, K = K, A = A, pi = pi)
}

#' Draws a simulated sample from a Hidden Markov Model
#'
#' @param T Length of the sequence
#' @param K Number of hidden states
#' @param A Transition matrix of size KxK
#' @param pi piial state probability vector of size Kx1
#' @param obsSim A function that draws a simulated sample from the observation
#' model. It takes only one argument, a vector of discrete states between 1 and
#' K, and returns a vector of same size containing the sampled observations.
#'
#' @return A list with three elements:
#' *z* the vector of hidden states, and
#' *y* the vector of observations.
#' @export
#'
#' @examples
hmm_sim <- function(T, K, A, pi, obsSim) {
  if (!is.matrix(A) || any(dim(A) != K))
    stop("A must be a KxK matrix.")

  if (any(rowSums(A) != 1))
    stop("The rows of A must add up to 1.")

  if (min(A) < 0 || max(A) > 1)
    stop("The elements of A can only take values between the interval [0, 1].")

  if (length(pi) != K)
    stop("The vector pi must have length K.")

  z <- vector("numeric", T)
  z[1] <- sample(x = 1:K, size = 1, replace = FALSE, prob = pi)
  for (t in 2:T) {
    z[t] <- sample(x = 1:K, size = 1, replace = FALSE, prob = A[z[t - 1], ])
  }

  y <- do.call(obsSim, list(z))

  list(
    z = z,
    y = y
  )
}

