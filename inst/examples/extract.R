# Models ------------------------------------------------------------------
mySpec <- hmm(
  K = 3, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Gaussian"
)

set.seed(9000)
y <- as.matrix(
  c(rnorm(100, 5, 1), rnorm(100, 0, 1), rnorm(100, -5, 1))
)

myFit <- fit(mySpec, y = y, chains = 4, iter = 500, seed = 9000)

mySpec <- hmm(
  K = 3, R = 2,
  observation = MVGaussian(
    mu    = Gaussian(mu = 0, sigma = 100),
    sigma = Gaussian(mu = 0, sigma = 10)
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multivariate Gaussian"
)

set.seed(9000)
y = rbind(
  MASS::mvrnorm(n = 100, mu = c(-10,  -5), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)),
  MASS::mvrnorm(n = 100, mu = c(  0,   0), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)),
  MASS::mvrnorm(n = 100, mu = c( 10,   5), Sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2))
)

myModel <- compile(mySpec)
myOptim <- optimizing(mySpec, myModel, y = y, nRun = 50, nCores = 4, keep = "best", as_vector = FALSE)

# Fitted via sampling -----------------------------------------------------
str(
  extract_quantity(myFit, pars = "mu11")
)

str(
  extract_quantity(myFit, pars = c("mu11", "mu21"))
)

str(
  extract_quantity(myFit, pars = c("mu11", "mu21"), reduce = median)
)

str(
  extract_quantity(myFit, pars = c("mu11", "mu21"), reduce = posterior_intervals(c(0.1, 0.9)))
)

str(
  extract_quantity(myFit, pars = c("mu11", "mu21"), combine = c)
)

str(
  extract_quantity(myFit, pars = c("mu11", "mu21"), reduce = median, combine = c)
)

str(
  extract_quantity(myFit, pars = c("mu11", "mu21"), reduce = median, combine = rbind)
)

str(
  extract_quantity(myFit, pars = c("mu11", "mu21"), reduce = median, combine = `+`)
)

str(
  extract_quantity(
    myFit,
    pars    = c("mu11", "mu21"),
    reduce  = posterior_intervals(c(0.1, 0.9)),
    combine = rbind
  )
)

str(
  extract_quantity(myFit, pars = "zstar")
)

str(
  extract_quantity(myFit, pars = "zstar", reduce = posterior_mode)
)

str(
  extract_quantity(myFit, pars = "zstar", reduce = posterior_mode, chain = 1)
)

str(
  extract_alpha(myFit)
)

str(
  extract_alpha(myFit, reduce = median)
)

str(
  extract_zstar(myFit)
)

str(
  extract_zstar(myFit, reduce = posterior_mode)
)

str(
  extract_zstar(myFit, reduce = posterior_mode, chain = 1)
)

# Fitted via optimizing ---------------------------------------------------
str(
  extract_quantity(myOptim, pars = "mu1")
)

str(
  extract_quantity(myOptim, pars = "mu", combine = c)
)

str(
  extract_quantity(myOptim, pars = "mu", reduce = identity, combine = c)
)

str(
  extract_quantity(myOptim, pars = "mu", reduce = identity, combine = sum)
)

str(
  extract_quantity(myOptim, pars = "zstar")
)

str(
  extract_zstar(myOptim)
)

str(
  extract_alpha(myOptim)
)

# Extracting states -------------------------------------------------------
str(
  extract_quantity(myFit, pars = "alpha")
)

# all.equal(extract_quantity(myFit, pars = "alpha"), extract_quantity(myFit, pars = "alpha", chain = "all"))

str(
  extract_quantity(myFit, pars = "alpha", chain = 1)
)

# all.equal(extract_quantity(myFit, pars = "alpha")[[1]][, 1, , ], extract_quantity(myFit, pars = "alpha", chain = 1)[[1]])

str(
  extract_quantity(myFit, pars = "alpha", reduce = median, chain = 1)
)

str(
  extract_quantity(myOptim, pars = "alpha")
)

str(
  extract_quantity(myFit, pars = c("alpha", "gamma"))
)

str(
  extract_quantity(myOptim, pars = c("alpha", "gamma"))
)

# Other extractors --------------------------------------------------------
extract_time(myOptim)
extract_time(myFit)

extract_R(myOptim)
extract_K(myOptim)
extract_T(myOptim)
extract_y(myOptim)
extract_data(myOptim)
extract_filename(myOptim)

# Plots -------------------------------------------------------------------

plot_series(myFit, legend.cex = 0.8)

plot_series(myFit, xlab = "Time steps", features = c("yColoredLine"))

plot_series(myFit, stateProbability =  "smoothed", features = c("stateShade", "bottomColoredMarks"))

plot_state_probability(myFit, main = "Title", xlab = "Time")

plot_state_probability(myFit, features = c("stateShade"), main = "Title", xlab = "Time")

plot_state_probability(myFit, features = c("bottomColoredMarks"), main = "Title", xlab = "Time")

plot_state_probability(myFit, features = c("probabilityColoredDots"), main = "Title", xlab = "Time")

plot_state_probability(myFit, features = c("probabilityColoredLine"), main = "Title", xlab = "Time")

plot_state_probability(myFit, stateProbability = "filtered", features = c("bottomColoredMarks", "probabilityFan"), stateProbabilityInterval = posterior_intervals(c(0.05, 0.95)), main = "Title", xlab = "Time")

plot_ppredictive(myFit, type = c("density", "cumulative", "summary"), fun = median)

plot_ppredictive(myFit, type = c("density", "boxplot"), fun = median, subset = 1:10)

plot_ppredictive(myFit, type = c("density", "boxplot", "scatter"), fun = median, fun1 = mean, fun2 = median, subset = 1:40)

plot_ppredictive(myFit, type = c("density", "cumulative", "hist"), fun = median)

plot_ppredictive(myFit, type = c("density", "cumulative", "ks"))

plot_ppredictive(myOptim)

plot_series(myOptim, legend.cex = 0.8)

plot_series(myOptim, xlab = "Time steps", features = c("yColoredLine"))

plot_series(myOptim, stateProbability =  "smoothed", features = c("stateShade", "bottomColoredMarks"))

plot_state_probability(myOptim, main = "Title", xlab = "Time")

plot_state_probability(myOptim, features = c("stateShade"), main = "Title", xlab = "Time")

plot_state_probability(myOptim, features = c("bottomColoredMarks"), main = "Title", xlab = "Time")

plot_state_probability(myOptim, features = c("probabilityColoredDots"), main = "Title", xlab = "Time")

plot_state_probability(myOptim, features = c("probabilityColoredLine"), main = "Title", xlab = "Time")

plot_state_probability(myOptim, stateProbability = "filtered", features = c("bottomColoredMarks"), main = "Title", xlab = "Time")

