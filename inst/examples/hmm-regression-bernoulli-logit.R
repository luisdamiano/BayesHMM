mySpec <- hmm(
  K = 2, R = 1,
  observation = RegBernoulliLogit(
    xBeta = Gaussian(mu = 0, sigma = 10),
    M     = 3
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5)),
  name = "Univariate Bernoulli Logistic Regression"
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
    rbinom(150, 1, prob = exp(x[  1:150, ] %*% c(-1, -0.7,  1.5)) / (1 + exp(x[  1:150, ] %*% c(-1, -0.7,  1.5)))),
    rbinom(150, 1, prob = exp(x[151:300, ] %*% c( 2,  3.8, -4.5)) / (1 + exp(x[151:300, ] %*% c( 2,  3.8, -4.5))))
  )
)

myFit <- fit(mySpec, y = y, x = x, chains = 1, iter = 500, seed = 9000)

plot_series(myFit)

print_fit(myFit)

