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
  MASS::mvrnorm(n = 100, mu = c(-10,  -5), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)),
  MASS::mvrnorm(n = 100, mu = c(  0,   0), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)),
  MASS::mvrnorm(n = 100, mu = c( 10,   5), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2))
)

myModel <- compile(mySpec)
myBest  <- optimizing(mySpec, myModel, y = y, nRun = 50, nCores = 4, keep = "best", as_vector = FALSE)

extract_grid(myBest, pars = "mu")

print_obs(myBest)

plot_obs(myBest)
