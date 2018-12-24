mySpec <- hmm(
  K = 3, R = 1,
  observation =
    Gaussian(mu = Gaussian(mu = -10, sigma = 1), sigma = 1) +
    Gaussian(mu = Gaussian(mu =   0, sigma = 1), sigma = 1) +
    Gaussian(mu = Gaussian(mu =  10, sigma = 1), sigma = 1),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Gaussian"
)

set.seed(9000)
y <- as.matrix(
  c(rnorm(100, -10, 1), rnorm(100, 0, 1), rnorm(100, 10, 1))
)

myModel <- compile(mySpec)
myFit   <- drawSamples(
  mySpec, stanModel = myModel, y = y,
  chains = 1, iter = 500, seed = 9000
)
myOpt   <- optimizing(mySpec, stanModel = myModel, y = y, nRuns = 20, seed = 9000)

head(extract_parameter_names(myFit))

head(extract_parameter_names(myOpt))

select_parameters(myFit, TRUE, FALSE, FALSE)

select_parameters(myOpt, TRUE, FALSE, FALSE)

extract_quantity(myFit)

extract_quantity(myOpt)

extract_quantity(myFit, pars = "mu11")

extract_quantity(myOpt, pars = "mu11")

extract_obs_parameters(myFit)

extract_obs_parameters(myOpt) ### breaks

extract_zstar(myFit, fun = median)

extract_zstar(myOpt) ### breaks

extract_grid(myOpt, pars = c("mu11", "mu21"))
