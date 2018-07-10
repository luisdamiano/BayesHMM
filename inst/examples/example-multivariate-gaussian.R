mySpec <- hmm(
  K = 3, R = 2,
  observation = MVGaussian(
    mu    = Gaussian(mu = 0, sigma = 100),
    sigma = Default()
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multivariate Gaussian"
)

mySpec <- hmm(
  K = 3, R = 2,
  observation = MVGaussian(
    mu    = Gaussian(mu = 0, sigma = 100),
    sigma = Gaussian(mu = 0, sigma = 10)
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multivariate Gaussian"
)

set.seed(9000)
y = rbind(
  MASS::mvrnorm(n = 100, mu = c(-10, -30), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)),
  MASS::mvrnorm(n = 100, mu = c(  0,   0), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)),
  MASS::mvrnorm(n = 100, mu = c( 10,  30), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2))
)

myFit <- fit(mySpec, y = y, x = x, chains = 1, iter = 500, seed = 9000)

plot_obs(myFit)

print_all(myFit)
