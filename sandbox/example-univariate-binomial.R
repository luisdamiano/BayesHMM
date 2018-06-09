library(rstan)

mySpec <- hmm(
  K = 3, R = 1,
  observation = Binomial(
    theta = Beta(alpha = 0.5, beta = 0.5, bounds = list(0, 1)),
    N     = Default(bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "My binomial model!..."
)

set.seed(9000)
myData <- list(
  y = as.matrix(
    c(rbinom(100, 100, 0.5), rbinom(100, 100, 0.2), rbinom(100, 100, 0.8))
  ),
  T = 300
)

myFit <- fit(mySpec, myData, chains = 1, iter = 500)

rstan::plot(myFit, pars = c("mu11", "mu21", "mu31"))

print(summary(myFit)[[1]][1:18, ], digits = 2)
