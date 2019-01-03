mySpec <- hmm(
  K = 2, R = 1,
  observation = RegBinomialLogit(
    xBeta = Gaussian(mu = 0, sigma = 10),
    M     = 3,
    N     = 10
  ),
  initial     = Dirichlet(alpha = c(1, 1)),
  transition  = Dirichlet(alpha = c(1, 1)),
  name = "Univariate Binomial Logistic Regression"
)

set.seed(9000)
x <- as.matrix(
  cbind(
    rep(1, 300),
    rnorm(300),
    rnorm(300)
  )
)
y <- as.matrix(
  c(
    rbinom(150, 10, prob = exp(x[  1:150, ] %*% c(-1, -0.7,  1.5)) / (1 + exp(x[  1:150, ] %*% c(-1, -0.7,  1.5)))),
    rbinom(150, 10, prob = exp(x[151:300, ] %*% c( 2,  3.8, -4.5)) / (1 + exp(x[151:300, ] %*% c( 2,  3.8, -4.5))))
  )
)

myModel <- compile(mySpec)
myFit   <- fit(mySpec, y = y, x = x, chains = 1, iter = 500, seed = 9000)

print(myFit)
