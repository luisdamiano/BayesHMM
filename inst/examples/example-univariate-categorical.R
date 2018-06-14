library(rstan)

mySpec <- hmm(
  K = 3, R = 1,
  observation = Categorical(
    theta = Dirichlet(alpha = c(0.5, 0.5, 0.5, 0.5)),
    N = 4
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Categorical"
)

set.seed(9000)
y = as.matrix(
  c(
    sample(1:4, size = 100, replace = TRUE, prob = c(0.1, 0.1, 0.1, 0.7)),
    sample(1:4, size = 100, replace = TRUE, prob = c(0.1, 0.1, 0.7, 0.1)),
    sample(1:4, size = 100, replace = TRUE, prob = c(0.1, 0.7, 0.1, 0.1))
  )
)

myFit <- fit(mySpec, y = y, chains = 1, iter = 500)

rstan::plot(myFit, pars = c("theta11", "theta21", "theta31"))

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)
