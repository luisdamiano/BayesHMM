library(BayesHMM)

mySpec <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Gaussian"
)

myModel <- compile(mySpec)

set.seed(9000)
for (i in 1:10) {
  y <- as.matrix(
    c(rnorm(100, 5, 1), rnorm(100, 0, 1), rnorm(100, -5, 1))
  )

  myFit <- draw_samples(
    mySpec, stanModel = myModel, y = y,
    chains = 1, iter = 500
  )

  print_obs(myFit)
}
