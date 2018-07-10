mySpec <- hmm(
  K = 3, R = 1,
  observation = NegativeBinomial(
    alpha = Gaussian(mu = 0, sigma = 10, bounds = list(0, NULL)),
    beta  = Gaussian(mu = 0, sigma = 10, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Negative Binomial (location) Model"
)

set.seed(9000)
y = as.matrix(
  c(
    rnbinom(1000, size = 1, mu = 1),
    rnbinom(1000, size = 1, mu = 10),
    rnbinom(1000, size = 1, mu = 5)
  )
)

myFit <- fit(mySpec, y = y, chains = 1, iter = 500, seed = 9000)

plot_obs(myFit)

print_all(myFit)

rbind(
  sort(c(pars[1]/pars[2], pars[3]/pars[4], pars[5]/pars[6])),
  sort(c(mean(myData$y[1:1000]), mean(myData$y[1001:2000]), mean(myData$y[2001:3000])))
)
