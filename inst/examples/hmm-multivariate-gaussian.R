mySpec <- hmm(
  K = 3, R = 2,
  observation = MVGaussian(
    mu    = MVGaussian(mu = c(0, 0), sigma = matrix(c(100, 0, 0, 100), 2, 2)),
    sigma = Wishart(nu = 5, sigma = matrix(c(1, 0, 0, 1), 2, 2))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multivariate Gaussian"
)

set.seed(9000)
y = rbind(
  MASS::mvrnorm(n = 100, mu = c( -1,  1), Sigma = matrix(c(1, 0, 0, 1), 2, 2)),
  MASS::mvrnorm(n = 100, mu = c(  0,  0), Sigma = matrix(c(1, 0, 0, 1), 2, 2)),
  MASS::mvrnorm(n = 100, mu = c( -3,  3), Sigma = matrix(c(1, 0, 0, 1), 2, 2))
)

myModel <- compile(mySpec)
myFit   <- sampling(mySpec, stanModel = myModel, y = y, chains = 1, iter = 500, seed = 9000)
print_obs(myFit)
