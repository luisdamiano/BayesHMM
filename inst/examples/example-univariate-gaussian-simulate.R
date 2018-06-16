library(rstan)

mySpec <- hmm(
  K = 3, R = 1,
  observation =
    Gaussian(mu = -5, sigma = 1)
    + Gaussian(mu = 0, sigma = 1)
    + Gaussian(mu = 5, sigma = 1),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Gaussian Generative"
)

set.seed(9000)
myFit <- run(mySpec, chains = 1, iter = 500)
# Alternatively, you can set the number of generated quantities T
# run(mySpec, T = 200, chains = 1, iter = 500)

plot(apply(extract(myFit, pars = "ypred")[[1]], 1, mean), type = "l")

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)

str(extract(myFit, pars = "ypred"))
