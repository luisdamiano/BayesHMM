mySpec <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10, ordered = TRUE),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL), equal = TRUE)
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Hidden Markov Model with ordered parameters"
)

BayesHMM:::write_model(mySpec, FALSE, "out")

set.seed(9000)
y <- as.matrix(
  c(rnorm(100, 5, 1), rnorm(100, 0, 1), rnorm(100, -5, 1))
)

explain(mySpec)

myModel <- compile(mySpec)
myFit   <- draw_samples(mySpec, myModel, y = y, chains = 1, iter = 500, seed = 9000)
myOpt   <- optimizing(mySpec, myModel, y = y, nRun = 50, nCores = 10, seed = 9000)

print(myFit)

# Hard-classify observations based on filtered, smoothed, and Viterbi
classify_alpha(myFit)

classify_gamma(myFit)

classify_zstar(myFit)

