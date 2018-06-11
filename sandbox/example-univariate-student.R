library(rstan)

mySpec <- hmm(
  K = 3, R = 1,
  observation = Student(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 2, bounds = list(0, NULL)),
    nu    = Cauchy(mu = 0, sigma = 10, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.1, 0.5, 1)),
  transition  = Dirichlet(alpha = c(0.1, 0.5, 1)),
  name = "My Student model!..."
)

set.seed(9000)
y = as.matrix(
  c(
    rt(100, df = 5, ncp = 0),
    rt(100, df = 5, ncp = 10),
    rt(100, df = 5, ncp = -10)
  )
)

myFit <- fit(mySpec, y = y, chains = 1, iter = 500, writeDir = "sandbox//out")

rstan::plot(myFit, pars = c("mu11", "mu21", "mu31"))

print(summary(myFit)[[1]][1:21, ], digits = 2)

str(extract(myFit, par = "ypred")[[1]])
