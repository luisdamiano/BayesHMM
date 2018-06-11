library(rstan)

mySpec <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = 0.5
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "My simple model!..."
)

set.seed(9000)
myData <- list(
  y = as.matrix(
    c(rnorm(100, 5, 1), rnorm(100, 0, 1), rnorm(100, -5, 1))
  ),
  T = 300
)

myFit <- fit(mySpec, myData, chains = 1, iter = 500)

rstan::plot(myFit, pars = c("mu11", "mu21", "mu31"))

print(summary(myFit)[[1]][1:18, ], digits = 2)

browseURL(stan_file(myFit))
