mySpec  <- hmm(
  K = 3, R = 1,
  observation =
    Gaussian(mu =  10, sigma = 1) +
    Gaussian(mu =   0, sigma = 1) +
    Gaussian(mu = -10, sigma = 1),
  initial     = InitialFixed(pi = c(0.2, 0.2, 0.6)),
  transition  = Dirichlet(alpha = c(1, 1, 1)),
  name = "Fixed Transition - Univariate Gaussian"
)

mySim   <- sim(mySpec, T = 500, chain = 1, nSimulations = 1, seed = 9000)
y       <- extract_ysim(mySim)

mySpec  <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = InitialFixed(pi = c(0.2, 0.2, 0.6)),
  transition  = Dirichlet(alpha = c(1, 1, 1)),
  name = "Univariate Gaussian"
)

myModel <- compile(mySpec)
myFit   <- draw_samples(mySpec, myModel, y = y, chains = 1, iter = 500, seed = 9000)

print(myFit)
