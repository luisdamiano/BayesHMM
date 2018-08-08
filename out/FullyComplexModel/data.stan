
    int<lower = 1> K; // number of hidden states
    int<lower = 1> R; // dimension of the observation vector
    

      int<lower = 1> Q;     // number of initial model predictors
      vector[Q] v;          // initial model predictors
      

      int<lower = 1> P;     // number of transition model predictors
      matrix[T, P] u;       // transition model predictors
      
matrix[T, R] y;  // observations
