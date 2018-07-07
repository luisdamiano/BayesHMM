mySpec <- hmm(
  K = 3, R = 2,
  observation =
    Gaussian(mu = Gaussian(mu = -10, sigma = 1), sigma = 1) +
    Gaussian(mu = Gaussian(mu =   0, sigma = 1), sigma = 1) +
    Gaussian(mu = Gaussian(mu =  10, sigma = 1), sigma = 1),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Dirichlet(alpha = c(1.0, 0.2, 0.2)) +
    Dirichlet(alpha = c(0.2, 1.0, 0.2)) +
    Dirichlet(alpha = c(0.2, 0.2, 1.0)),
  name = "Univariate Gaussian"
)

mySim <- sim(mySpec, T = 500, chain = 1, iter = 500, seed = 9000)
y     <- extract_ypred(mySim)[1, , ]
myFit <- fit(mySpec, y = y, chain = 1, iter = 500, seed = 9000)

