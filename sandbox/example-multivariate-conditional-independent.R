library(rstan)

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
  name = "My simple model!..."
)

set.seed(9000)
myData <- list(
  y = rbind(
    MASS::mvrnorm(n = 100, mu = c(-10, -30), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)),
    MASS::mvrnorm(n = 100, mu = c(  0,   0), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)),
    MASS::mvrnorm(n = 100, mu = c( 10,  30), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2))
  ),
  T = 300
)

write_model(mySpec, noLogLike = FALSE, writeDir = "sandbox//out")

myFit <- fit(mySpec, myData, chains = 1, iter = 500)

rstan::plot(myFit, pars = c("mu1", "mu2", "mu3"))

print(summary(myFit)[[1]][1:18, ], digits = 2)
