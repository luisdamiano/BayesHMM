hmm <- function(...) {
  x <- spec(...)
  class(x) <- append(class(x), "HMMSpecification", 0)
  x
}

block_functions.HMMSpecification <- function(spec) {
  "
  #include hmm-forward.stan
  #include hmm-forwardBackward.stan
  #include hmm-mappath.stan
  #include hmm-zpredictive.stan
  "
}

block_parameters.HMMSpecification <- function(spec) {
  "
  simplex[K] pi;                    // initial state probabilities
  simplex[K] A[K];                  // transition probabilities
                                    // A[i][j] = p(z_t = j | z_{t-1} = i)
  "
}

block_tparameters.HMMSpecification <- function(spec) {
  "
  vector[T] logalpha[K];
  vector[K] logpi;
  vector[K] logA[K];

  // 1. Fill initial distribution vector (if needed)
  logpi = log(pi);

  // 2. Fill transition matrix (if needed)
  logA = log(A);
  "
}

block_generated.HMMSpecification <- function(spec) {
  "
  vector[T] alpha[K];
  vector[T] gamma[K];
  int<lower=1, upper=K> zstar[T];

  for (t in 1:T)
    alpha[, t] = to_array_1d(softmax(to_vector(logalpha[, t])));

  gamma = forwardbackward(K, T, logpi, logA, loglike, alpha);
  zstar = MAPpath(K, T, logpi, logA, loglike);
  "
}

chunk_calculate_target.HMMSpecification <- function(spec) {
  "
  logalpha = forward(K, T, logpi, logA, loglike);
  "
}

chunk_increase_target.HMMSpecification <- function(spec) {
  "
  target += log_sum_exp(logalpha[, T]);
  "
}

chunk_zpredictive.HMMSpecification <- function(spec) {
  "
  zpred = zpredictive_rng(K, T, pi, A);
  "
}
