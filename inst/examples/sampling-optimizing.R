mySpec <- hmm(
  K = 2, R = 1,
  observation = Gaussian(-5, 1) + Gaussian(5, 1),
  initial     = Dirichlet(alpha = c(1, 1)),
  transition  = Dirichlet(alpha = c(1, 1)),
  name = "Univariate Gaussian Hidden Markov Model"
)

set.seed(9000)
y <- as.matrix(
  c(rnorm(150, -5, 1), rnorm(150,  5, 0.1))
)

myModel <- compile(mySpec)
myFit   <- draw_samples(
  mySpec, stanModel = myModel, y = y,
  chains = 1, iter = 500, seed = 9000
)
myAll   <- optimizing(mySpec, myModel, y = y, nRuns = 50, nCores = 10, seed = 9000, keep = "all")
myOpt   <- extract_best(myAll)

print(myFit)

print(myOpt)
