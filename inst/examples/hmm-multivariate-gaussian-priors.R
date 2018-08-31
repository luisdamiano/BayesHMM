mySpec <- hmm(
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
  name = "Multivariate Gaussian"
)

browse_model(mySpec)

set.seed(9000)
y = rbind(
  MASS::mvrnorm(n = 100, mu = c(-10, -10), Sigma = matrix(c(1, 0, 0, 1), 2, 2)),
  MASS::mvrnorm(n = 100, mu = c(  0,   0), Sigma = matrix(c(1, 0, 0, 1), 2, 2)),
  MASS::mvrnorm(n = 100, mu = c( 10,  10), Sigma = matrix(c(1, 0, 0, 1), 2, 2))
)

myModel <- compile(mySpec)
myFit   <- sampling(mySpec, stanModel = myModel, y = y, chains = 1, iter = 500, seed = 9000)
print_obs(myFit)
