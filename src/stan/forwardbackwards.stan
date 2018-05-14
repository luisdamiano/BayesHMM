  // Normalize
  vector normalize(real[] x) {
    return to_vector(x) / sum(x);
  }

  // Forward-backward algorithm log p(z_t = j | x_{1:T})
  vector[] forwardbackward(int K, int T, vector logpi, vector[] logA, vector[] loglike, vector[] alpha) {
    vector[T] logbeta[K];
    vector[T] loggamma[K];
    vector[T] beta[K];
    vector[T] gamma[K];
    real accumulator[K];

    for (j in 1:K) {
      logbeta[j, T] = 1;
    }

    for (tforward in 0:(T-2)) {
      int t;
      t = T - tforward;

      for (j in 1:K) { // j = previous (t-1)
        for (i in 1:K) { // i = next (t)
          accumulator[i] = logbeta[i, t] + logA[j, i] + loglike[i, t];
        }
        logbeta[j, t-1] = log_sum_exp(accumulator);
      }
    }

    for (t in 1:T) {
      beta[, t] = to_array_1d(softmax(to_vector(logbeta[, t])));
    }

    for(t in 1:T) {
      loggamma[, t] = to_array_1d(to_vector(alpha[, t]) .* to_vector(beta[, t]));
    }

    for(t in 1:T) {
      gamma[, t] = to_array_1d(normalize(loggamma[, t]));
    }

    return gamma;
  } // Forward-ackward
