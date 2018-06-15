mixture <- function(...) {
  x <- spec(...)
  class(x) <- append(class(x), "MixtureSpecification", 0)
  x
}

block_functions.MixtureSpecification <- function(spec) {
  ""
}

block_parameters.MixtureSpecification <- function(spec) {
  "
  simplex[K] pi;                   // mixing proportion
  "
}

block_tparameters.MixtureSpecification <- function(spec) {
  "
  vector[K] logpi;
  logpi = log(pi);
  "
}

block_generated.MixtureSpecification <- function(spec) {
  ""
}

chunk_calculate_target.MixtureSpecification <- function(spec) {
  ""
}

chunk_increase_target.MixtureSpecification <- function(spec) {
  "
  for (t in 1:T) {
    vector[K] accumulator = logpi;
    for (k in 1:K) {
      accumulator[k] += loglike[k, t];
    }
    target += log_sum_exp(accumulator);
  }
  "
}

chunk_zpredictive.MixtureSpecification <- function(spec) {
  "
  for(t in 1:T) {
    zpred[t] = categorical_rng(pi);
  }
  "
}
