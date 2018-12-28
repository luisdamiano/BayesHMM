mySpec  <- hmm(
  K = 3, R = 1,
  observation =
    Gaussian(mu =  10, sigma = 1) +
    Gaussian(mu =   0, sigma = 1) +
    Gaussian(mu = -10, sigma = 1),
  initial     = Dirichlet(alpha = c(1, 1, 1)),
  transition  = TransitionFixed(
    A = matrix(c(0.2, 0.3, 0.1, 0.4, 0.2, 0.7, 0.4, 0.5, 0.2), nrow = 3)
  ),
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
  initial     = Dirichlet(alpha = c(1, 1, 1)),
  transition  = TransitionFixed(
    A = matrix(c(0.2, 0.3, 0.1, 0.4, 0.2, 0.7, 0.4, 0.5, 0.2), nrow = 3)
  ),
  name = "Univariate Gaussian"
)

myModel <- compile(mySpec)
myFit   <- draw_samples(mySpec, myModel, y = y, chains = 1, iter = 500, seed = 9000)

print(myFit)
