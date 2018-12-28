mySpec <- hmm(
  K = 2, R = 1,
  observation = RegBinomialProbit(
    xBeta = Gaussian(mu = 0, sigma = 10),
    M     = 3,
    N     = 10
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5)),
  name = "Univariate Binomial Probit Regression"
)

set.seed(9000)
x <- as.matrix(
  cbind(
    rep(1, 300),
    rnorm(300),
    rnorm(300)
  )
)

mySims  <- sim(mySpec, T = 300, x = x, nSimulations = 1, seed = 9000)
ySim    <- extract_ysim(mySims)[1, ]
myModel <- compile(mySpec)
myFit   <- fit(mySpec, y = ySim, x = x, chains = 1, iter = 500, seed = 9000)

print(myFit)
