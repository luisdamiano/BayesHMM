mySpec <- hmm(
  K = 3, R = 2,
  observation = # Different densities per state
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 100, bounds = list(0, NULL))
    )
    + Cauchy(
      mu    = Gaussian(0.01, 10),
      sigma = Gaussian(0, 100, bounds = list(0, NULL))
    )
    + Student(
      mu    = Gaussian(-0.01, 10),
      sigma = Gaussian(0, 100, bounds = list(0, NULL)),
      nu    = Default(bounds = list(0, NULL))
    ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multivariate Conditional Independent Density per State"
)

set.seed(9000)
y = rbind(
  MASS::mvrnorm(n = 100, mu = c(-10, -30), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)),
  MASS::mvrnorm(n = 100, mu = c(  0,   0), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)),
  MASS::mvrnorm(n = 100, mu = c( 10,  30), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2))
)

myFit <- fit(mySpec, y = y, chains = 1, iter = 500, seed = 9000)

plot_obs(myFit)

print_all(myFit)
