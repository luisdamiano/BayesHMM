source('R/density.R')
source('R/specification.R')

mySpec1 <- list(
  name = "More complex model!",
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
