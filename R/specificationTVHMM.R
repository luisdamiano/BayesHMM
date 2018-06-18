tvhmm <- function(...) {
  x <- spec(...)
  class(x) <- append(class(x), "TVHMMSpecification", 0)
  x
}

block_functions.TVHMMSpecification <- function(spec) {
  "
  #include tvhmm-forward.stan
  #include tvhmm-forwardBackward.stan
  #include tvhmm-mappath.stan
  #include tvhmm-zpredictive.stan
  "
}

block_data.TVHMMSpecification <- function(spec) {
  "
  int<lower = 1> S; // number of transition model predictors
  matrix[T, S] s;   // transition model predictors
  "
}

block_parameters.TVHMMSpecification <- function(spec) {
  "
  simplex[K] pi;                    // initial state probabilities
  matrix[K, S] sBeta[K];            // transition model regressors
                                    // sBeta[to, from, s regressors]
  "
}

block_tparameters.TVHMMSpecification <- function(spec) {
  "
  vector[K] A[T, K];
  vector[T] logalpha[K];
  vector[K] logpi;
  vector[K] logA[T, K];             // transition logA[t, from, to]

  // 1. Fill initial distribution vector (if needed)
  logpi = log(pi);

  // 2. Fill transition matrix (if needed)
  for (t in 1:T) {
    for (i in 1:K) { // i = previous (t-1)
      A[t, i] = softmax((s[t] * sBeta[i]')');
    }
    logA[t] = log(A[t]);
    //for (j in 1:K) { // j = current (t)
    //  vector[K] transitionCol = softmax((s[t] * sBeta[j]')');
    //  for (i in 1:K) { // i = previous (t-1)
    //    A[t][i, j] = transitionCol[i];
    //  }
    //  logA[t] = log(A[t]);
    //A[t][ , j] = softmax((s[t] * sBeta[j]')');
    //logA[t][ , j] = log(A[t, , j]);
    //}
  }
  "
}

block_generated.TVHMMSpecification <- function(spec) {
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

chunk_calculate_target.TVHMMSpecification <- function(spec) {
  "
  logalpha = forward(K, T, logpi, logA, loglike);
  "
}

chunk_increase_target.TVHMMSpecification <- function(spec) {
  "
  target += log_sum_exp(logalpha[, T]);
  "
}

chunk_zpredictive.TVHMMSpecification <- function(spec) {
  "
  zpred = zpredictive_rng(K, T, pi, A);
  "
}
