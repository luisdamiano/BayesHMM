mySpec <- hmm(
  K = 3, R = 1,
  observation = Student(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 2, bounds = list(0, NULL)),
    nu    = Cauchy(mu = 0, sigma = 10, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.1, 0.5, 1)),
  transition  = Dirichlet(alpha = c(0.1, 0.5, 1)),
  name = "Univariate Student Model"
)

set.seed(9000)
y = as.matrix(
  c(
    rt(100, df = 5, ncp =   0),
    rt(100, df = 5, ncp =  10),
    rt(100, df = 5, ncp = -10)
  )
)

myFit <- fit(mySpec, y = y, chains = 1, iter = 500, seed = 9000)

plot_series(myFit)

print_fit(myFit)

