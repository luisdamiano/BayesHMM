mySpec <- hmm(
  K = 3, R = 1,
  observation = Beta(
    alpha = ImproperUniform(bounds = list(0, NULL)),
    beta  = ImproperUniform(bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Beta (proportion) Model"
)

# mySpec <- hmm(
#   K = 3, R = 1,
#   observation = Beta(
#     alpha = Gaussian(mu = 0, sigma = 10, bounds = list(0, NULL)),
#     beta  = Gaussian(mu = 0, sigma = 10, bounds = list(0, NULL))
#   ),
#   initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
#   transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
#   name = "Univariate Beta (proportion) Model"
# )

set.seed(9000)
y = as.matrix(
  c(
    rbeta(100, 0.5, 0.5),
    rbeta(100, 0.5, 1.5),
    rbeta(100, 1.5, 0.5)
  )
)

myFit <- fit(mySpec, y = y, chains = 1, iter = 500, seed = 9000)

plot_series(myFit)

print_fit(myFit)
