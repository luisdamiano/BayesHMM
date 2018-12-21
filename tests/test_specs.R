# Univariate continuous output --------------------------------------------

# Univariate Gaussian
mySpec1 <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Gaussian"
)

# Univariate Student
mySpec2 <- hmm(
  K = 3, R = 1,
  observation = Student(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 2, bounds = list(0, NULL)),
    nu    = Cauchy(mu = 0, sigma = 10, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.1, 0.5, 1)),
  transition  = Dirichlet(alpha = c(0.1, 0.5, 1)),
  name = "Univariate Student"
)

# Univariate Beta
mySpec3 <- hmm(
  K = 3, R = 1,
  observation = Beta(
    alpha = Default(bounds = list(0, NULL)),
    beta  = Default(bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Beta"
)

# Univariate discrete output ----------------------------------------------

# Univariate Poisson
mySpec4 <- hmm(
  K = 3, R = 1,
  observation = Poisson(
    lambda = Default(bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Poisson"
)

# Univariate Categorical
mySpec5 <- hmm(
  K = 3, R = 1,
  observation = Categorical(
    theta = Dirichlet(alpha = c(0.5, 0.5, 0.5, 0.5)),
    N = 4
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Categorical"
)

# Univariate Binomial
mySpec6 <- hmm(
  K = 3, R = 1,
  observation = Binomial(
    theta = Beta(alpha = 0.5, beta = 0.5, bounds = list(0, 1)),
    N     = 100
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Binomial"
)

# Univariate Bernoulli
mySpec7 <- hmm(
  K = 3, R = 1,
  observation = Bernoulli(
    theta = Beta(alpha = 0.5, beta = 0.5, bounds = list(0, 1))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Bernoulli"
)

# Univariate Negative Binomial
mySpec7 <- hmm(
  K = 3, R = 1,
  observation = NegativeBinomial(
    alpha  = Default(bounds = list(0, NULL)),
    beta   = Default(bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Negative Binomial"
)

# Multivariate discrete output --------------------------------------------
mySpec8 <- hmm(
  K = 3, R = 4,
  observation = Multinomial(
    theta = Default(),
    N = 10
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multinomial"
)

# Multivariate continuous output ------------------------------------------

# Multivariate Gaussian
mySpec9 <- hmm(
  K = 3, R = 2,
  observation = MVGaussian(
    mu    = MVGaussian(mu = c(0, 0), sigma = matrix(c(100, 0, 0, 100), 2, 2)),
    sigma = Wishart(nu = 5, sigma = matrix(c(1, 0, 0, 1), 2, 2))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multivariate Gaussian"
)

# Multivariate Gaussian Fixed Covariance
mySpec10 <- hmm(
  K = 3, R = 2,
  observation = # Priors vary per state
    MVGaussian(
      mu    = MVGaussian(mu = c(-10, -10), sigma = matrix(c(100, 0, 0, 100), 2, 2)),
      sigma = matrix(c(1, 0, 0, 1), 2, 2)
    ) +
    MVGaussian(
      mu    = MVGaussian(mu = c(  0,   0), sigma = matrix(c(100, 0, 0, 100), 2, 2)),
      sigma = matrix(c(1, 0, 0, 1), 2, 2)
    ) +
    MVGaussian(
      mu    = MVGaussian(mu = c( 10,  10), sigma = matrix(c(100, 0, 0, 100), 2, 2)),
      sigma = matrix(c(1, 0, 0, 1), 2, 2)
    ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multivariate Gaussian Fixed Covariance"
)

# Multivariate Gaussian Correlation Cholesky Factor
mySpec11 <- hmm(
  K = 3, R = 2,
  observation = MVGaussianCor(
    mu    = MVGaussian(mu = c(0, 0), sigma = matrix(c(100, 0, 0, 100), 2, 2)),
    L     = LKJCor(eta = 2)
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multivariate Gaussian Correlation Cholesky Factor"
)

# Multivariate Gaussian Covariance Cholesky Factor
mySpec12 <- hmm(
  K = 3, R = 2,
  observation = MVGaussianCov(
    mu    = MVGaussian(mu = c(0, 0), sigma = matrix(c(100, 0, 0, 100), 2, 2)),
    L     = chol(matrix(c(1, 0, 0, 1), 2, 2))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multivariate Gaussian Covariance Cholesky Factor"
)

# Regression models -------------------------------------------------------

# Univariate Gaussian Regression
mySpec13 <- hmm(
  K = 2, R = 1,
  observation = RegGaussian(
    xBeta = Default(),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL)),
    M     = 3
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5)),
  name = "Univariate Gaussian Regression"
)

# Univariate Binomial Logistic Regression
mySpec14 <- hmm(
  K = 2, R = 1,
  observation = RegBernoulliLogit(
    xBeta = Gaussian(mu = 0, sigma = 10),
    M     = 3
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5)),
  name = "Univariate Bernoulli Logistic Regression"
)

# Univariate Binomial Probit Regression
mySpec15 <- hmm(
  K = 2, R = 1,
  observation = RegBinomialProbit(
    xBeta = Default(),
    M     = 3,
    N     = 100
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5)),
  name = "Univariate Binomial Probit Regression"
)

# Univariate Categorical Softmax Regression
mySpec16 <- hmm(
  K = 2, R = 1,
  observation = RegCategoricalSoftmax(
    xBeta = MVGaussian(mu = c(0, 0, 0), sigma = Default()),
    M     = 3,
    N     = 5 # different categories
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5)),
  name = "Univariate Categorical Softmax Regression"
)

# Crazy priors ------------------------------------------------------------

# Multivariate Gaussian as prior
mySpec17 <- hmm(
  K = 3, R = 2,
  observation = MVGaussian(
    mu    = MVGaussian(mu = c(0, 0), sigma = matrix(c(100, 0, 0, 100), 2, 2)),
    sigma = matrix(c(1, 0, 0, 1), 2, 2)
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multivariate Gaussian as prior"
)

# Fixed Parameters
mySpec18 <- hmm(
  K = 3, R = 1,
  observation =
    Gaussian(mu = -5, sigma = 1)
  + Gaussian(mu =  0, sigma = 1)
  + Gaussian(mu =  5, sigma = 1),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Gaussian Fixed Parameters"
)

# Tests for complex models ------------------------------------------------

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
      vBeta = Default()
    ),
  transition  = Default(),
  name = "TV Initial distribution"
)

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

vars  <- ls()
specs <- vars[grepl(glob2rx("mySpec*"), vars) | grepl(glob2rx("exCase*"), vars)]
for (spec in specs) {
  error_in_write_model(get(spec))
}
