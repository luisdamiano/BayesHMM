library(rstan)

mySpec <- hmm(
  K = 3, R = 1,
  observation = Poisson(
    lambda = Default()
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "My simple model!..."
)

set.seed(9000)
myData <- list(
  y = as.matrix(
    c(rpois(100, 5), rpois(100, 10), rpois(100, 1))
  ),
  T = 300
)

myFit <- fit(mySpec, myData, chains = 1, iter = 500)

rstan::plot(myFit, pars = c("mu11", "mu21", "mu31"))

print(summary(myFit)[[1]][1:18, ], digits = 2)
