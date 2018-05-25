  // Prior/posterior predictive
  int[] zpredictive_rng(int K, int T, vector pi, vector[] A) {
    int zpred[T];

    // Sample initial state
    zpred[1] = categorical_rng(pi);

    for(t in 2:T) {
      zpred[t] = categorical_rng(A[zpred[t-1]]);
    }

    return zpred;
  }
