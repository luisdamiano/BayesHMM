functions {
  #include forward.stan
  #include forwardbackwards.stan
  #include MAPpath.stan
  #include zpredictive.stan
}

data {
  int<lower=1> T;                   // number of observations (length)
  #include data.stan
}

transformed data {
  // Constants
  #include constants.stan
}

parameters {
  // Discrete state model
  simplex[K] pi;                    // initial state probabilities
  simplex[K] A[K];                  // transition probabilities
                                    // A[i][j] = p(z_t = j | z_{t-1} = i)

  // Observation model
  #include parameters.stan
}

transformed parameters {
  vector[K] logpi;
  vector[K] logA[K];
  vector[T] loglike[K];
  vector[T] logalpha[K];

  // 1. Fill initial distribution vector (if needed)
  logpi = log(pi);

  // 2. Fill transition matrix (if needed)
  logA = log(A);

  // 3. Fill loglikelihood (always needed)
  for (t in 1:T) {
    #include loglikelihood.stan
  }

  // 4. Compute forward quantity
  logalpha = forward(K, T, logpi, logA, loglike);
}

model {
  // 1. Initial distribution vector priors
  #include priors.stan

  // Go!
  target += log_sum_exp(logalpha[, T]);
}

generated quantities {
  vector[T] alpha[K];
  vector[T] gamma[K];
  vector[T] ypred;
  int<lower=1, upper=K> zstar[T];
  int<lower=1, upper=K> zpred[T];

  for (t in 1:T)
    alpha[, t] = to_array_1d(softmax(to_vector(logalpha[, t])));

  gamma = forwardbackward(K, T, logpi, logA, loglike, alpha);
  zstar = MAPpath(K, T, logpi, logA, loglike);
  zpred = zpredictive_rng(K, T, pi, A);

  for(t in 1:T) {
    #include ypredictive.stan
  }
}
