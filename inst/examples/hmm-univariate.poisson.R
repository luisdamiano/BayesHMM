mySpec <- hmm(
  K = 2, R = 1,
  observation = Poisson(
    lambda = ImproperUniform(bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Poisson Model"
)

set.seed(9000)
y = as.matrix(
  c(rpois(150, 10), rpois(150, 1))
)

myModel <- compile(mySpec)
myFit   <- draw_samples(
  mySpec, stanModel = myModel, y = y,
  chains = 3, iter = 600, seed = 9000
)
myAll   <- optimizing(mySpec, myModel, y = y, nRuns = 5, nCores = 10, seed = 9000, keep = "all")
myOpt   <- extract_best(myAll)

plot(myFit)

plot(myOpt)
