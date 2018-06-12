library(rstan)

mySpec <- hmm(
  K = 3, R = 1,
  observation = Beta(
    alpha = Default(bounds = list(0, NULL)),
    beta  = Default(bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Beta (proportion) Model"
)

set.seed(9000)
y = as.matrix(
  c(
    rbeta(100, 0.5, 0.5),
    rbeta(100, 0.5, 1.5),
    rbeta(100, 1.5, 0.5)
  )
)

myFit <- fit(mySpec, y = y, chains = 1, iter = 500)

rstan::plot(myFit, pars = c("alpha11", "alpha21", "alpha31"))

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)
