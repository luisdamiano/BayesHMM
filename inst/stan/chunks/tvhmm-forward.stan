// Forward algorithm log p(z_t = j | x_{1:t})
vector[] forward(int K, int T, vector logpi, vector[, ] logA, vector[] loglike) {
  vector[T] logalpha[K];
  real accumulator[K];

  for(j in 1:K) {
    logalpha[j, 1] = logpi[j] + loglike[j, 1];
  }

  for (t in 2:T) {
    for (j in 1:K) { // j = current (t)
    for (i in 1:K) { // i = previous (t-1)
    accumulator[i] = logalpha[i, t-1] + logA[t, i, j] + loglike[j, t];
    }
    logalpha[j, t] = log_sum_exp(accumulator);
    }
  }

  return logalpha;
}
