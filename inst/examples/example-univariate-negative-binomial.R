library(rstan)

# mySpec <- spec(
#   K = 3, R = 1,
#   observation = NegativeBinomial(
#     mu  = Gaussian(mu = 0, sigma = 10, bounds = list(0, NULL)),
#     phi = Gaussian(mu = 0, sigma = 10, bounds = list(0, NULL))
#   ),
#   initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
#   transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
#   name = "Univariate Negative Binomial (location) Model"
# )
#
# set.seed(9000)
# y = as.matrix(
#   c(
#     rnbinom(1000, size = 1, mu = 1),
#     rnbinom(1000, size = 1, mu = 10),
#     rnbinom(1000, size = 1, mu = 5)
#   )
# )
#
# myFit <- run(mySpec, y = y, chains = 1, iter = 500)
#
# rstan::plot(myFit, pars = c("mu11", "mu21", "mu31"))
#
# print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)
