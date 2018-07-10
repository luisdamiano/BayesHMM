stop("Example currently not working.")

mySpec <- hmm(
  K = 2, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5)),
  transition  = TransitionSoftmax(
    uBeta = Gaussian(0, 10), P = 2
  ),
  name = "TVHMM Univariate Gaussian"
)

set.seed(9000)
u <- cbind(
  rep(1, 300),
  rnorm(300)
)

mySim <- sim(mySpec, u = u, T = 300, iter = 500, chains = 1)
ySim  <- extract_ypred(mySim, permuted = FALSE)
y     <- ySim[1, 1, ]
myFit <- fit(mySpec, y = y, u = u, iter = 500, chains = 1)

rstan::plot(myFit, pars = c("mu11", "mu21", "mu31"))

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)
