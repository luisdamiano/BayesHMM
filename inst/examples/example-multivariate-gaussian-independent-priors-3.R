# library(rstan)
#
# mySpec <- hmm(
#   K = 3, R = 2,
#   observation = MVGaussian(
#     mu    = Gaussian(mu = 0, sigma = 100) + Student(mu = 0, sigma = 100, nu = 10),
#     sigma = Default()
#   ),
#   initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
#   transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
#   name = "Multivariate Gaussian with independent priors 3"
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
