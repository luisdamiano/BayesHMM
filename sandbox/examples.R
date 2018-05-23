library(rstan)

mySpec <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "My simple model!..."
)

set.seed(9000)
myData <- list(
  x = c(rnorm(100, 5, 1), rnorm(100, 0, 1), rnorm(100, -5, 1)),
  T = 300
)

myFit <- fit(mySpec, myData, chains = 1, iter = 500)

rstan::plot(myFit, pars = c("mu1", "mu2", "mu3"))

print(summary(myFit)[[1]][1:18, ], digits = 2)

# myModel1a <- hmm(
#   K = 3, R = 1,
#   observation = Gaussian(
#     mu    = Gaussian(0, 10),
#     sigma = Student(mu = 0, sigma = 10, nu = 2)
#   ),
#   initial     = Dirichlet(alpha = c(0.1, 0.5, 1)),
#   transition  = Dirichlet(alpha = c(0.1, 0.5, 1)),
#   name = "My simple model!..."
# )

# write_model(myModel1a, "out")

myModel1s <- hmm(
  K = 3, R = 1,
  observation = Student(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 2),
    nu    = Default()
  ),
  initial     = Dirichlet(alpha = c(0.1, 0.5, 1)),
  transition  = Dirichlet(alpha = c(0.1, 0.5, 1)),
  name = "My Student model!..."
)

# write_model(myModel1s, "out")

myModel1b <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(mu = 0, sigma = 10, bounds = list(0, 5)),
    sigma = Gaussian(0, 10, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.1, 0.5, 1)),
  transition  = Dirichlet(alpha = c(0.1, 0.5, 1)),
  name = "Model with bounds ##!..."
)

# write_model(myModel1b, "out")

myModel1c <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(mu = 0, sigma = 10, bounds = list(0, 5), trunc = list(-5, 5)),
    sigma = Gaussian(0, 10, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.1, 0.5, 1)),
  transition  = Dirichlet(alpha = c(0.1, 0.5, 1)),
  name = "Model with bounds and truncs ##!..."
)

# write_model(myModel1c, "out")

myModel2 <- hmm(
  K = 3, R = 1,
  observation = # Different priors per state
    Gaussian(
      mu    = Gaussian(1, 10),
      sigma = Default()
    )
  + Gaussian(
    mu    = Gaussian(2, 10),
    sigma = Fixed(value = 0.5)
  )
  + Gaussian(
    mu    = Gaussian(3, 10),
    sigma = Gaussian(0, 100)
  ),
  initial     = # Different prior for each element of the init prob vector
    Beta(alpha = 0.5, beta = 0.5)
  + Beta(alpha = 0.2, beta = 0.2)
  + Beta(alpha = 0.3, beta = 0.3),
  transition  = # Different prior for each row of the transition matrix
    Dirichlet(alpha = c(0.1, 0.5, 1))
  + Dirichlet(alpha = c(2.1, 2.5, 3.1))
  + Dirichlet(alpha = c(3.1, 3.5, 4.1)),
  name = "Complex model-"
)

# write_model(myModel2, "out")
#
# tmp <- fit(myModel1, data = list(
#   x = rnorm(100),
#   T = 100
#   )
# )
#
# rstan::stan(
#   file = "out/Mysimplemodel/model.stan",
#   data = list(
#     x = rnorm(100),
#     T = 100
#   )
# )
