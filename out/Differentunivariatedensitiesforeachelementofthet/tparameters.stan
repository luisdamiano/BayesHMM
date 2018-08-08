
    vector[T] logalpha[K];
    vector[K] logpi;
    
vector[K] A[T, K];
		vector[K] logA[T, K];						 // transition logA[t, from, to]
logpi = log(pi);

        for (t in 1:T) {
          for (i in 1:K) { // i = previous (t-1)
            #include transitionLink.stan
          }
          logA[t] = log(A[t]);
        }
      
