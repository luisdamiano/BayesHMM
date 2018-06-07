library(rstan)

mySpec <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Gaussian Generative"
)

set.seed(9000)
myFit <- fit(mySpec, chains = 1, iter = 500, writeDir = "sandbox/out")
# Alternatively, you can set the number of generated quantities T
# fit(mySpec, T = 200, chains = 1, iter = 500)

rstan::plot(myFit, pars = c("mu1", "mu2", "mu3"))

print(summary(myFit)[[1]][1:18, ], digits = 2)

str(extract(myFit, pars = "ypred"))
