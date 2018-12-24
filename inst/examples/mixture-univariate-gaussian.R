mySpec <- mixture(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = Default()),
  name = "Univariate Gaussian Mixture"
)

set.seed(9000)
y <- as.matrix(
  c(rnorm(50, 5, 1), rnorm(300, 0, 1), rnorm(100, -5, 1))
)

myModel <- compile(mySpec)
myFit   <- drawSamples(mySpec, myModel, y = y, chains = 1, iter = 500)
myOpt   <- optimizing(mySpec, myModel, y = y, nRun = 20, keep = "all", nCores = 4)
# myFit   <- fit(mySpec, myModel, y = y, chains = 1, iter = 500)

myBest  <- extract_best(myOpt)

plot_series(myBest)

print_obs(myBest)

plot_series(myFit)

print_fit(myFit)
