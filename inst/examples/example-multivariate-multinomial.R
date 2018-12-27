library(rstan)

mySpec <- hmm(
  K = 3, R = 4,
  observation = Multinomial(
    theta = ImproperUniform(),
    N = 10
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multinomial"
)

set.seed(9000)
y = as.matrix(
  t(
    cbind(
      rmultinom(100, 10, c(0.1, 0.1, 0.1, 0.7)),
      rmultinom(100, 10, c(0.1, 0.1, 0.7, 0.1)),
      rmultinom(100, 10, c(0.1, 0.7, 0.1, 0.1))
    )
  )
)

myFit <- run(mySpec, data = make_data(mySpec, y), chains = 1, iter = 500)

rstan::plot(myFit, pars = c("theta11", "theta21", "theta31"))

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)

str(extract(myFit, pars = "ypred")[[1]])
