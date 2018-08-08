
    vector[T] alpha[K];
    vector[T] gamma[K];
    int<lower=1, upper=K> zstar[T];

    for (t in 1:T)
    alpha[, t] = to_array_1d(softmax(to_vector(logalpha[, t])));

    gamma = forwardbackward(K, T, logpi, logA, loglike, alpha);
    zstar = MAPpath(K, T, logpi, logA, loglike);
    
