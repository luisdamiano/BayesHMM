  // Mixing algorithm
  vector[] mixture(int K, int T, vector logpi, vector[] loglike) {
    vector[T] logalpha[K];
    real accumulator[K];

  for (t in 1:T) {
    real lps[K] = log_theta;
    for (k in 1:K) {
      lps[k] += normal_lpdf(y[n] | mu[k], sigma[k]);
    }
    target += log_sum_exp(lps);
  }

    return logalpha;
  }
