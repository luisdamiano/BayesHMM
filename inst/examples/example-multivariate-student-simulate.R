library(rstan)

mySpec <- spec(
  K = 3, R = 2,
  observation = MVStudent(
    nu    = 3,
    mu    = c(-5, 5),
    sigma = matrix(c(1, 0.5, 0.5, 1), 2, 2)
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multivariate Student"
)

set.seed(9000)
myFit <- run(mySpec, chains = 1, iter = 500)

matplot(
  apply(extract(myFit, pars = "ypred")[[1]], c(2, 3), mean),
  type = "l"
)

rstan::plot(myFit, pars = c("mu1", "mu2", "mu3"))

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)
