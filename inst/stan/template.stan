functions {
  #include functions.stan
}

data {
  int<lower=1> T;                   // number of observations (length)
  #include data.stan
}

transformed data {
  #include tdata.stan
  #include constants.stan
  #include fixedParameters.stan
}

parameters {
  #include parameters.stan
  #include freeParameters.stan
}

transformed parameters {
  vector[T] loglike[K];
  #include tparameters.stan

  // Compute loglikelihood
  for (t in 1:T) {
    #include logLikelihood.stan
  }

  // Compute target quantity
  #include calculate-target.stan
}

model {
  #include priors.stan
  #include increase-target.stan
}

generated quantities {
  matrix[T, R] ypred;
  int<lower=1, upper=K> zpred[T];

  #include generated.stan
  #include zpredictive.stan
  for(t in 1:T) {
    #include ypredictive.stan
  }
}
