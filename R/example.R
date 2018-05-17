source('R/density.R')
source('R/specification.R')
source('R/utils.R')

mySpec1 <- list(
  name = "### My complex model!...",
  K = 3,  # Number of states
  observation = list(
    R = 1,  # Dimension of the observation vector
    covariates = NULL,
    density = list( # Allows 1 or K elements only.
      k1 = list( # Allows 1 or R elements only.
        k1r1 = Gaussian(
          mu    = Gaussian(mu = 1.1, sigma = 1 , k = 1, r = "", param = "mu"),
          sigma = Gaussian(mu = 1.2, sigma = 10, k = 1, r = "", param = "sigma"),
          k = 1, r = "", param = "k1r"
        )
      ),
      k2 = list( # Allows 1 or R elements only.
        k2r1 = Gaussian(
          mu    = Gaussian(mu = 2.1, sigma = 2 , k = 2, r = "", param = "mu"),
          sigma = Gaussian(mu = 2.2, sigma = 20, k = 2, r = "", param = "sigma"),
          k = 2, r = "", param = "k2r"
        )
      ),
      k3 = list( # Allows 1 or R elements only.
        k3r1 = Gaussian(
          mu    = Gaussian(mu = 3.1, sigma = 3 , k = 3, r = "", param = "mu"),
          sigma = Gaussian(mu = 3.2, sigma = 30, k = 3, r = "", param = "sigma"),
          k = 3, r = "", param = "k3r"
        )
      )
    )
  ),
  init_prob = list(
    density = list( # Allows 1 or K elements only.
      k1 = Beta(alpha = 0.1, beta = 0.01, k = "[1]", r = "", param = "pi"),
      k2 = Beta(alpha = 0.2, beta = 0.02, k = "[2]", r = "", param = "pi"),
      k3 = Beta(alpha = 0.3, beta = 0.03, k = "[3]", r = "", param = "pi")
    )
  ),
  transition = list(
    covariates = NULL,
    density = list( # Allows 1, K, or KxK elements only.
      k1 = list( # Allows 1 or K elements only.
        k1k1 = Beta(alpha = 0.0, beta = 0.0, k = "[1, 1]", r = "", param = "A"),
        k1k2 = Fixed(value = 0.5, k = "[1, 2]", r = "", param = "A"), # Fixed values
        k1k3 = Default(k = "[1, 3]", r = "", param = "A") # Stan default priors
      ),
      k2 = list( # Allows 1 or K elements only.
        k2k  = Beta(alpha = 0.0, beta = 0.0, k = "[2, ]", r = "", param = "A")
      ),
      k3 = list( # Allows 1 or K elements only.
        k3k1 = Fixed(value = 0.1, k = "[3, 1]", r = "", param = "A"),
        k3k2 = Fixed(value = 0.2, k = "[3, 2]", r = "", param = "A"),
        k3k3 = Fixed(value = 0.7, k = "[3, 3]", r = "", param = "A")
      )
    )
  )
)

class(mySpec1) <- "Specification"
write_model(mySpec1, "out")

mySpec2 <- hmm(
  K = 3,
  R = 1,
  observation = Gaussian(mu = 1.1, sigma = 1 , k = 1, r = "", param = "mu"),
  initial = Fixed(value = 0.1, k = "[3, 1]", r = "", param = "A"),
  transition = Fixed(value = 0.1, k = "[3, 1]", r = "", param = "A")
)

# I. a) One univariate density
# Action: Same density for each output dimension in each state
# Does it share parameters?
obs1 <- parse_observation(
  K = 3, R = 3,
  Gaussian(mu = 1.1, sigma = 1 , k = 1, r = "", param = "mu")
)

# I. b) One multivariate density
# Action: Same multivariate density for each state
obs2 <- parse_observation(
  K = 3, R = 3,
  MVGaussian(mu = 1.1, sigma = 1 , k = 1, r = "", param = "mu")
)

# I. c) K univariate densities
# Action: Same density for each output dimension inside a state
obs3 <- parse_observation(
  K = 3, R = 3,
  Gaussian(mu = 1.1, sigma = 1 , k = 1, r = "", param = "mu")
  + Gaussian(mu = 2.1, sigma = 2 , k = 2, r = "", param = "mu")
  + Gaussian(mu = 3.1, sigma = 3 , k = 3, r = "", param = "mu")
)

# I. d) K multivariate densities
# Action: One density for each state
obs4 <- parse_observation(
  K = 3, R = 3,
  MVGaussian(mu = 1.1, sigma = 1 , k = 1, r = "", param = "mu")
  + MVGaussian(mu = 2.1, sigma = 2 , k = 2, r = "", param = "mu")
  + MVGaussian(mu = 3.1, sigma = 3 , k = 3, r = "", param = "mu")
)

# I. e) K x R univariate densities (i.e. different densities for each dimension of the obs vector, plus different parameters for different states)
# TO BE IMPLEMENTED

# II. a) One univariate density
init1 <- parse_initial(
  K = 3,
  Beta(alpha = 0.5, beta = 0.5)
)

# II. b) One multivariate density
init2 <- parse_initial(
  K = 3,
  Dirichlet(alpha = c(0.1, 0.1, 0.1))
)

# II. c) K univariate density
init3 <- parse_initial(
  K = 3,
  Beta(alpha = 0.1, beta = 0.1)
  + Beta(alpha = 0.5, beta = 0.5)
  + Beta(alpha = 0.9, beta = 0.9)
)

# II. d) K multivariate density
init4 <- parse_initial(
  K = 3,
  Dirichlet(alpha = c(0.1, 0.1, 0.1))
  + Dirichlet(alpha = c(0.1, 0.1, 0.1))
  + Dirichlet(alpha = c(0.1, 0.1, 0.1))
) # Error! Doesn't make sense

# III. a) One univariate density
trans1 <- parse_transition(
  K = 3,
  Beta(alpha = 0.1, beta = 0.1)
)

# III. b) One multivariate density
trans2 <- parse_transition(
  K = 3,
  Dirichlet(alpha = c(0.1, 0.1, 0.1))
)

# III. c) K univariate densities
trans3 <- parse_transition(
  K = 3,
  Beta(alpha = 0.1, beta = 0.1)
  + Beta(alpha = 0.5, beta = 0.5)
  + Beta(alpha = 0.9, beta = 0.9)
)

# III. d) K multivariate densities
trans4 <- parse_transition(
  K = 3,
  Dirichlet(alpha = c(0.1, 0.1, 0.1))
  + Dirichlet(alpha = c(0.1, 0.1, 0.1))
  + Dirichlet(alpha = c(0.1, 0.1, 0.1))
)






myModel <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Gaussian(0, 10)
    ),
  initial     = Dirichlet(alpha = c(0.1, 0.5, 1), k = 3, r = "", param = "pi"),
  transition  = Dirichlet(alpha = c(0.1, 0.5, 1)),
  name = "My simple model."
)

write_model(myModel, "out")
