library(rstan)

mySpec <- hmm(
  K = 2, R = 1,
  observation = RegBinomialProbit(
    xBeta = Default(),
    M     = 3,
    N     = 100
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
y <- as.matrix(
  c(
    rbinom(150, 100, prob = pnorm(x[  1:150, ] %*% c(-1, -0.7,  1.5))),
    rbinom(150, 100, prob = pnorm(x[151:300, ] %*% c( 2,  3.8, -4.5)))
  )
)

myData  <- make_data(
  spec  = mySpec,
  y     = y,
  xBeta = x
)

myFit <- run(mySpec, data = myData, chains = 1, iter = 500, writeDir = "out")

rstan::plot(myFit, pars = c("xBeta11", "xBeta21"))

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)

# glm(cbind(y[1:150, ], 100 - y[1:150, ]) ~ x[1:150, -1], family = binomial(link = "logit"))
# glm(cbind(y[1:150, ], 100 - y[1:150, ]) ~ x[1:150, -1], family = binomial(link = "probit"))
