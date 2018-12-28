mySpec  <- hmm(
  K = 2, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = InitialSoftmax(
    vBeta = Gaussian(0, 10), Q = 2
  ),
  transition  = Dirichlet(alpha = c(0.5, 0.5)),
  name = "TVHMM Univariate Gaussian"
)

v <- c(1, 0.5)

mySims  <- sim(mySpec, T = 1000, v = v, nSimulations = 500)
ySim    <- extract_ysim(mySims)[1, 1, ]
myModel <- compile(mySpec)
myFit   <- draw_samples(mySpec, myModel, y = ySim, v = v, iter = 500)
