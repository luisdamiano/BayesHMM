  // Viterbi arg_max ...
  int[] MAPpath(int K, int T, vector logpi, vector[] logA, vector[] loglike) {
    int zstar[T];
    real logp_zstar;
    int bpointer[T, K];
    real delta[T, K];

    for (j in 1:K)
      delta[1, j] = loglike[j, 1];

    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        delta[t, j] = negative_infinity();
        for (i in 1:K) { // i = previous (t-1)
          real logp;
          logp = delta[t-1, i] + logA[i, j] + loglike[j, t];
          if (logp > delta[t, j]) {
            bpointer[t, j] = i;
            delta[t, j] = logp;
          }
        }
      }
    }

    logp_zstar = max(delta[T]);

    for (j in 1:K) {
      if (delta[T, j] == logp_zstar) {
        zstar[T] = j;
      }
    }

    for (t in 1:(T - 1)) {
      zstar[T - t] = bpointer[T - t + 1, zstar[T - t + 1]];
    }

    return zstar;
  } // Viterbi
