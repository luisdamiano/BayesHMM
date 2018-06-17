library(rstan)

mySpec <- tvhmm(
  K = 2, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5)),
  transition  = Default(),
  name = "Univariate Gaussian"
)

set.seed(9000)
s <- cbind(
  rep(1, 300),
  rnorm(300)
)

xBeta <- matrix(
  c(0.5, 0.3, -0.5, -0.3),
  ncol = 2, nrow = 2
)

y <- as.matrix(
  c(rnorm(150, 5, 1), rnorm(150, 0, 1))
)

myData <- list(
  y = y,
  T = NROW(y),
  K = 2,
  S = 2,
  s = s,
  R = 1
)

myFit <- run(mySpec, data = myData, chains = 1, iter = 500, writeDir = "out")

rstan::plot(myFit, pars = c("mu11", "mu21", "mu31"))

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)
