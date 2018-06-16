library(rstan)

mySpec <- hmm(
  K = 3, R = 1,
  observation = Bernoulli(
    theta = Beta(alpha = 0.5, beta = 0.5, bounds = list(0, 1))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Bernoulli Model"
)

set.seed(9000)
y = as.matrix(
  c(
    sapply(1:1000, function(x) { rbinom(1, 1, 0.5) }),
    sapply(1:1000, function(x) { rbinom(1, 1, 0.2) }),
    sapply(1:1000, function(x) { rbinom(1, 1, 0.8) })
  )
)

myFit <- run(mySpec, data = make_data(mySpec, y), chains = 1, iter = 500)

rstan::plot(myFit, pars = c("theta11", "theta21", "theta31"))

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)
