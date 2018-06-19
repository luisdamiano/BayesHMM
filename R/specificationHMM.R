hmm <- function(...) {
  x <- spec(...)
  class(x) <- append(class(x), "HMMSpecification", 0)
  x
}

block_functions.HMMSpecification <- function(spec) {
  if (is.TVTransition(spec)) {
    "
    #include tvhmm-forward.stan
    #include tvhmm-forwardBackward.stan
    #include tvhmm-mappath.stan
    #include tvhmm-zpredictive.stan
    "
  } else {
    "
    #include hmm-forward.stan
    #include hmm-forwardBackward.stan
    #include hmm-mappath.stan
    #include hmm-zpredictive.stan
    "
  }
}

block_data.HMMSpecification <- function(spec) {
  if (is.TVTransition(spec)) {
    "
    int<lower = 1> S; // number of transition model predictors
    matrix[T, S] s;   // transition model predictors
    "
  } else {
    ""
  }
}

block_parameters.HMMSpecification <- function(spec) {
  strInitial <-
    if (is.TVInitial(spec)) {
      "
      simplex[K] pi;                    // initial state probabilities
      "
    } else {
      "
      simplex[K] pi;                    // initial state probabilities
      "
    }

  strTransition <-
    if (is.TVTransition(spec)) {
      # "
      #   matrix[K, S] sBeta[K];            // transition model regressors
      #                                     // sBeta[to, from, s regressors]
      # "
    } else {
      "
      simplex[K] A[K];                  // transition probabilities
                                        // A[i][j] = p(z_t = j | z_{t-1} = i)
      "
    }

  c(
    strInitial,
    strTransition
  )
}

block_tparameters.HMMSpecification <- function(spec) {
  if (is.TVTransition(spec)) {
    # A[t, i] = softmax((s[t] * sBeta[i]')');
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
        #include transitionLink.stan
      }
      logA[t] = log(A[t]);
    }
    "
  } else {
    "
    vector[T] logalpha[K];
    vector[K] logpi;
    vector[K] logA[K];

    logpi = log(pi);
    logA = log(A);
    "
  }
}

block_generated.HMMSpecification <- function(spec) {
  if (is.TVTransition(spec)) {
    "
    vector[T] alpha[K];
    vector[T] gamma[K];
    int<lower=1, upper=K> zstar[T];

    for (t in 1:T)
    alpha[, t] = to_array_1d(softmax(to_vector(logalpha[, t])));

    gamma = forwardbackward(K, T, logpi, logA, loglike, alpha);
    zstar = MAPpath(K, T, logpi, logA, loglike);
    "
  } else {
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
