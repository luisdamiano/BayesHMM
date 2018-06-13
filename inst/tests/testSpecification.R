test_empty_spec <- function() {
  checkException(
    hmm(),
    "Fails when creating an empty specification."
  )
}

test_insufficient_spec <- function() {
  checkException(
    hmm(K = 3, R = 1),
    "Fails when creating a specification without density."
  )
}

test_minimum_spec <- function() {
  mySpec <- hmm(
    K = 1, R = 1, observation = Gaussian()
  )

  checkTrue(
    error_on_write_model(mySpec),
    "Can create Stan code for a minimum specification."
  )
}

test_K_univariate_observation_densities <- function() {
  mySpec <- hmm(
    K = 3, R = 1,
    observation =
      Gaussian(
        mu    = Gaussian(0, 10),
        sigma = 0
      )
      + Student(
        mu    = Gaussian(0, 10),
        sigma = Default(),
        nu    = 3
      )
      + Cauchy(
        mu    = Gaussian(0, 10),
        sigma = Cauchy(0, 10)
      )
  )

  checkTrue(
    error_on_write_model(mySpec),
    "Can create Stan code for an observation model with K different univariate densities per state."
  )
}

test_nonK_univariate_observation_densities <- function() {
  checkException(
    hmm(
      K = 3, R = 1,
      observation =
        Gaussian(
          mu    = Gaussian(0, 10),
          sigma = 0
        )
        + Student(
          mu    = Gaussian(0, 10),
          sigma = Default(),
          nu    = 3
        )
    ),
    "Can create Stan code for an observation model with != K different univariate densities per state."
  )
}

test_illegal_observation_density_mixing <- function() {
  checkException(
    hmm(
      K = 2, R = 1,
      observation =
        Gaussian(
          mu    = Gaussian(0, 10),
          sigma = 0
        )
        + MVGaussian(
          mu    = Default(),
          sigma = Default()
        )
    ),
    "Fails when mixing univariate and multivariate densities in the observation model."
  )
}

test_fixed_parameters <- function() {
  mySpec <- hmm(
    K = 3, R = 1,
    observation = Gaussian(
      mu    = Gaussian(0, 10),
      sigma = 0
    )
  )

  checkTrue(
    error_on_write_model(mySpec),
    "Can create Stan code for an observation model with fixed parameters."
  )
}

test_fixed_parameters_scalar <- function() {
  mySpec <- hmm(
    K = 1, R = 1,
    observation = Gaussian(
      mu    = Gaussian(0, 10),
      sigma = 1
    )
  )

  checkTrue(
    error_on_write_model(mySpec),
    "Can create Stan code for an observation model with fixed scalar parameters."
  )
}

test_illegal_fixed_parameters_scalar <- function() {
  checkException(
    hmm(
      K = 1, R = 1,
      observation = Gaussian(
        mu    = Gaussian(0, 10),
        sigma = c(1, 0)
      )
    ),
    "Fails when an illegal scalar fixed parameter is given."
  )
}

test_fixed_parameters_vector <- function() {
  mySpec <- hmm(
    K = 1, R = 2,
    observation = MVGaussian(
      mu    = c(0, 10),
      sigma = Default()
    )
  )

  checkTrue(
    error_on_write_model(mySpec),
    "Can create Stan code for an observation model with fixed vector parameters."
  )
}

test_illegal_fixed_parameters_vector <- function() {
  checkException(
    hmm(
      K = 1, R = 1,
      observation = MVGaussian(
        mu    = matrix(1:2, ncol = 2, nrow = 1),
        sigma = Default()
      )
    ),
    "Fails when an illegal vector of fixed parameters is given."
  )
}

test_fixed_parameters_matrix <- function() {
  mySpec <- hmm(
    K = 1, R = 2,
    observation = MVGaussian(
      mu    = Gaussian(0, 10),
      sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)
    )
  )

  checkTrue(
    error_on_write_model(mySpec),
    "Can create Stan code for an observation model with a matrix of fixed parameters."
  )
}

test_illegal_fixed_parameters_matrix <- function() {
  checkException(
    hmm(
      K = 1, R = 1,
      observation = MVGaussian(
        mu    = Gaussian(0, 10),
        sigma = c(1, 2, 3, 4)
      )
    ),
    "Fails when an illegal matrix of fixed parameters is given."
  )
}
