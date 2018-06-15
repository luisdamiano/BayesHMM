library(rstan)

mySpec <- spec(
  K = 2, R = 1,
  observation = RegGaussian(
    xBeta = Default(),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL)),
    M     = 3
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5)),
  name = "Univariate Gaussian Regression"
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
    x[  1:150, ] %*% c(-1, -0.7,  1.5) + rnorm(150),
    x[151:300, ] %*% c( 2,  3.8, -4.5) + rnorm(150)
  )
)

myData  <- make_data(
  spec = mySpec,
  y = y,
  xBeta = x
)

myFit <- run(mySpec, data = myData, chains = 1, iter = 500, writeDir = "out")

rstan::plot(myFit, pars = c("xBeta11", "xBeta21"))

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)
