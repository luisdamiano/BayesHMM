# OBSERVATION MODEL -------------------------------------------------------

K = 3
R = 2

# Case 1. A different multivariate density for each state
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

# INITIAL MODEL -----------------------------------------------------------
K = 3
R = 2

# Case 9. Same univariate density for every initial state
#   Input: One univariate density
#   Behaviour: Repeat input K times
exCase9  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial =
    Beta(
      alpha = Gaussian(0, 1),
      beta  = Gaussian(1, 10)
    ),
  transition  = Default(),
  name = "Same univariate density for every initial state"
)

write_model(exCase9, noLogLike = FALSE, "out")

# Case 10. One multivariate density for the whole initial vector
#   Input: One multivariate density
#   Behaviour: Nothing
exCase10  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial =
    Dirichlet(
      alpha = Default()
    ),
  transition  = Default(),
  name = "One multivariate density for the whole initial vector"
)

write_model(exCase10, noLogLike = FALSE, "out")

# Case 11. A different univariate density for each initial state
#   Input: K univariate densities
#   Behaviour: Nothing
exCase11 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial =
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
  transition  = Default(),
  name = "A different univariate density for each initial state"
)

write_model(exCase11, noLogLike = FALSE, "out")

# Case 12. A link
#   Input: One link density
#   Behaviour: Nothing
exCase12 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial =
    InitialSoftmax(
      sBeta = Default()
    ),
  transition  = Default(),
  name = "TV Initial distribution"
)

write_model(exCase12, noLogLike = FALSE, "out")

# TRANSITION MODEL --------------------------------------------------------
K = 3
R = 2

# Case 13. Same univariate density for every transition
#   Input: One univariate density
#   Behaviour: Repeat input KxK times
exCase13 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Gaussian(mu = 0, sigma = 1),
  name = "Same univariate density for every transition"
)

write_model(exCase13, noLogLike = FALSE, "out")

# Case 14. Same multivariate density for every transition row
#   Input: One multivariate density
#   Behaviour: Repeat input K times

exCase14 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Dirichlet(
      alpha = c(0.5, 0.5, 0.7)
    ),
  name = "Same multivariate density for every transition row"
)

write_model(exCase14, noLogLike = FALSE, "out")

# Case 15. A different univariate density for each element of the transition row
#   Input: K univariate densities
#   Behaviour: Repeat input K times

exCase15 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Beta(alpha = 0.1, beta = 0.1) +
    Beta(alpha = 0.5, beta = 0.5) +
    Beta(alpha = 0.9, beta = 0.9),
  name = "A different univariate density for each element of the transition row"
)

write_model(exCase15, noLogLike = FALSE, "out")

# Case 16. A different multivariate density for each transition row
#   Input: K multivariate densities
#   Behaviour: nothing

exCase16 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Dirichlet(alpha = c(0.1, 0.1, 0.1)) +
    Dirichlet(alpha = c(0.5, 0.5, 0.5)) +
    Dirichlet(alpha = c(0.9, 0.9, 0.9)),
  name = "A different multivariate density for each transition row"
)

write_model(exCase16, noLogLike = FALSE, "out")

# Case 17. Different univariate densities for each element of the transition matrix
#   Input: KxK univariate densities
#   Behaviour: None

exCase17 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Beta(alpha = 0.1, beta = 0.1) +
    Beta(alpha = 0.2, beta = 0.2) +
    Beta(alpha = 0.3, beta = 0.3) +
    Beta(alpha = 0.4, beta = 0.4) +
    Beta(alpha = 0.5, beta = 0.5) +
    Beta(alpha = 0.6, beta = 0.6) +
    Beta(alpha = 0.7, beta = 0.7) +
    Beta(alpha = 0.8, beta = 0.8) +
    Beta(alpha = 0.9, beta = 0.9),
  name = "Different univariate densities for each element of the transition matrix"
)

write_model(exCase17, noLogLike = FALSE, "out")

# Case 18. A link
#   Input: One link density
#   Behaviour: Nothing

exCase18 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    TransitionSoftmax(
      sBeta = Gaussian(mu = 0, sigma = 1)
    ),
  name = "Different univariate densities for each element of the transition matrix"
)

write_model(exCase18, noLogLike = FALSE, "out")

# A FULLY COMPLEX MODEL ---------------------------------------------------

# Case 19. A link in both the transition and initial distribution.
#   Input: A link in both the transition and initial distribution.
#   Behaviour: Nothing
exCase19 <- hmm(
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
  initial =
    InitialSoftmax(
      sBeta = Gaussian(mu = 0, sigma = 1)
    ),
  transition  =
    TransitionSoftmax(
      sBeta = Gaussian(mu = 0, sigma = 1)
    ),
  name = "Fully Complex Model"
)

write_model(exCase19, noLogLike = FALSE, "out")
