mySpec <- hmm(
  K = 3, R = 1,
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
myFit <- fit(mySpec, y = y, chain = 1, iter = 500, seed = 9000)

plot_series(myFit, ylab = "Observation", legend.cex = 0.8)

plot_series(myFit, xlab = "Time", features = c("yColoredLine"))

plot_series(myFit, ylab = "Observation", state = "smoothed", features = c("stateShade", "bottomColoredMarks"))

plot_prob(myFit, main = "Title", xlab = "Time")

plot_prob(myFit, features = c("stateShade"), main = "Title", xlab = "Time")

plot_prob(myFit, features = c("bottomColoredMarks"), main = "Title", xlab = "Time")

plot_prob(myFit, features = c("probabilityColoredDots"), main = "Title", xlab = "Time")

plot_prob(myFit, features = c("probabilityColoredLine"), main = "Title", xlab = "Time")

plot_prob(myFit, state = "filtered", features = c("bottomColoredMarks", "probabilityFan"), probInterval = c(0.05, 0.95), main = "Title", xlab = "Time")

