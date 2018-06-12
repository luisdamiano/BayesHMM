library(rstan)

mySpec <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Default()
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Gaussian Fixed Sigma"
)

set.seed(9000)
y <- as.matrix(
  c(rnorm(100, 5, 1), rnorm(100, 0, 1), rnorm(100, -5, 1))
)

myFit <- fit(mySpec, y = y, chains = 1, iter = 500)

rstan::plot(myFit, pars = c("mu11", "mu21", "mu31"))

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)

browseURL(write_model(mySpec, noLogLike = FALSE, writeDir =  "sandbox//out"))
