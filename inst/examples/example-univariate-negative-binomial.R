mySpec <- hmm(
  K = 3, R = 1,
  observation = NegativeBinomial(
    alpha  = Default(bounds = list(0, NULL)),
    beta   = Default(bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Negative Binomial Model"
)

set.seed(9000)
y = as.matrix(
  c(
    rnbinom(1000, size = 1, mu = 1),
    rnbinom(1000, size = 1, mu = 10),
    rnbinom(1000, size = 1, mu = 5)
  )
)

myFit <- fit(mySpec, y = y, chains = 1, iter = 500, seed = 9000)

plot_obs(myFit)

print_all(myFit)

