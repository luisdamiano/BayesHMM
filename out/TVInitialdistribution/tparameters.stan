
    vector[T] logalpha[K];
    vector[K] logpi;
    
vector[K] pi;
		vector[K] logA[K];						 // transition logA[from, to]

      #include initialLink.stan
      

      logA = log(A);
      
