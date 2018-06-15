# myModel1c <- spec(
#   K = 3, R = 1,
#   observation = Gaussian(
#     mu    = Gaussian(mu = 0, sigma = 10, bounds = list(0, 5), trunc = list(-5, 5)),
#     sigma = Gaussian(0, 10, bounds = list(0, NULL))
#   ),
#   initial     = Dirichlet(alpha = c(0.1, 0.5, 1)),
#   transition  = Dirichlet(alpha = c(0.1, 0.5, 1)),
#   name = "Model with bounds and truncs ##!..."
# )
#
# myModel2 <- spec(
#   K = 3, R = 1,
#   observation = # Different priors per state
#     Gaussian(
#       mu    = Gaussian(1, 10),
#       sigma = Default()
#     )
#   + Gaussian(
#     mu    = Gaussian(2, 10),
#     sigma = Fixed(value = 0.5)
#   )
#   + Gaussian(
#     mu    = Gaussian(3, 10),
#     sigma = Gaussian(0, 100)
#   ),
#   initial     = # Different prior for each element of the init prob vector
#     Beta(alpha = 0.5, beta = 0.5)
#   + Beta(alpha = 0.2, beta = 0.2)
#   + Beta(alpha = 0.3, beta = 0.3),
#   transition  = # Different prior for each row of the transition matrix
#     Dirichlet(alpha = c(0.1, 0.5, 1))
#   + Dirichlet(alpha = c(2.1, 2.5, 3.1))
#   + Dirichlet(alpha = c(3.1, 3.5, 4.1)),
#   name = "Complex model-"
# )
