K = 3
R = 2

# MULTIVARIATE Observation model ------------------------------------------
# Case 1. A different multivariate density for each each state
#   Input: K multivariate densities
#   Behaviour: Nothing

exCase1  <- hmm(
  K = K, R = R,
  observation =
    MVGaussianCor(
      mu    = Gaussian(mu = 0, sigma = 1),
      L     = LKJCor(eta = 2)
    ) +
    MVGaussianCor(
      mu    = Gaussian(mu = 0, sigma = 10),
      L     = LKJCor(eta = 3)
    ) +
    MVGaussianCor(
      mu    = Gaussian(mu = 0, sigma = 100),
      L     = LKJCor(eta = 4)
    ),
  initial     = Default(),
  transition  = Default(),
  name = "A different multivariate density for each each state"
)

# Case 2. Same multivariate density for every state
#   Input: One multivariate density
#   Behaviour: Repeat input K times

exCase2  <- hmm(
  K = K, R = R,
  observation =
    MVGaussianCor(
      mu    = Gaussian(mu = 0, sigma = 100),
      L     = LKJCor(eta = 2)
    ),
  initial     = Default(),
  transition  = Default(),
  name = "Same multivariate density for every state"
)

# Case 3. Same univariate density for every state and every output variable
#   Input: One univariate density
#   Behaviour: Repeat input K %nested% R times

exCase3  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial     = Default(),
  transition  = Default(),
  name = "Same univariate density for every state and every output variable"
)

# Case 4. Same R univariate densities for every state
#   Input: R univariate densities
#   Behaviour: Repeat input K times

exCase4  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial     = Default(),
  transition  = Default(),
  name = "Same R univariate densities for every state"
)

# Case 5. Same univariate density for every output variable
#   Input: K univariate densities
#   Behaviour: Repeat input R times

exCase5  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial     = Default(),
  transition  = Default(),
  name = "Same univariate density for every output variable"
)

# Case 6. Different univariate densities for every pair of state and output variable
#   Input: K %nested% R univariate densities
#   Behaviour: None

exCase6  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial     = Default(),
  transition  = Default(),
  name = "Different univariate densities for every pair of state and output variable"
)

# UNIVARIATE Observation model --------------------------------------------
K = 3
R = 1
# Case 7. A different univariate density for each each state
#   Input: K univariate densities
#   Behaviour: Nothing

exCase7  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial     = Default(),
  transition  = Default(),
  name = "A different univariate density for each each state"
)

# Case 8. Same univariate density for every state
#   Input: One univariate density
#   Behaviour: Repeat input K times

exCase8  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial     = Default(),
  transition  = Default(),
  name = "Same multivariate density for every state"
)

