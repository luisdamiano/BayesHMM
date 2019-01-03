mySpec <- hmm(
  K = 3, R = 1,
  observation = Bernoulli(
    theta = Beta(alpha = 0.5, beta = 0.5, bounds = list(0, 1))
  ),
  initial     = Dirichlet(alpha = c(1, 1, 1)),
  transition  = Dirichlet(alpha = c(1, 1, 1)),
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

myFit <- fit(mySpec, y = y, chains = 1, iter = 500, seed = 9000)

plot_series(myFit)

print_fit(myFit)
