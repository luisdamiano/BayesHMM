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
y <- as.matrix(
  c(rnorm(100, 5, 1), rnorm(100, 0, 1), rnorm(100, -5, 1))
)

myModel <- compile(mySpec)
myFit   <- draw_samples(mySpec, myModel, y = y, chains = 1, iter = 500, seed = 9000)

print(myFit)
