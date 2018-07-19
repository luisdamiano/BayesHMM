mySpec <- hmm(
  K = 3, R = 2,
  observation =
    Gaussian(mu = -10, sigma = 1) +
    Gaussian(mu =   0, sigma = 1) +
    Gaussian(mu =  10, sigma = 1),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Dirichlet(alpha = c(1.0, 0.2, 0.2)) +
    Dirichlet(alpha = c(0.2, 1.0, 0.2)) +
    Dirichlet(alpha = c(0.2, 0.2, 1.0)),
  name = "Univariate Gaussian"
)

mySim <- sim(mySpec, T = 500, chain = 1, iter = 500, seed = 9000)
y     <- extract_ypred(mySim)[1, , ]
myFit <- fit(mySpec, y = y, chain = 4, iter = 500, seed = 9000)

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
