# library(rstan)
#
# mySpec <- hmm(
#   K = 3, R = 2,
#   observation = MVGaussianCor(
#     mu    = Student(nu = 10, mu = 0, sigma = 100) + Student(nu = 100, mu = 0, sigma = 1000),
#     L     = LKJCor(eta = 2)
#   ),
#   initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
#   transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
#   name = "Multivariate Gaussian with independent priors 2"
# )
#
# set.seed(9000)
# y = rbind(
#   MASS::mvrnorm(n = 100, mu = c(-10, -30), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)),
#   MASS::mvrnorm(n = 100, mu = c(  0,   0), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)),
#   MASS::mvrnorm(n = 100, mu = c( 10,  30), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2))
# )
#
# myFit <- fit(mySpec, y = y, chains = 1, iter = 500)
#
# rstan::plot(myFit, pars = c("mu1", "mu2", "mu3"))
#
# print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)
#
