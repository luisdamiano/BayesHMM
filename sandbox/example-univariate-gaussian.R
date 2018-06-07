library(rstan)

mySpec <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "My simple model!..."
)

set.seed(9000)
myData <- list(
  y = c(rnorm(100, 5, 1), rnorm(100, 0, 1), rnorm(100, -5, 1)),
  T = 300
)

myFit <- fit(mySpec, myData, chains = 1, iter = 500)

rstan::plot(myFit, pars = c("mu1", "mu2", "mu3"))

print(summary(myFit)[[1]][1:18, ], digits = 2)
