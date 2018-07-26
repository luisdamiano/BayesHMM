mySpec <- hmm(
  K = 3, R = 2,
  observation =
    MVGaussian(
      mu    = c( 0,  0),
      sigma = matrix(c(1, 0, 0, 1), 2, 2)
    ) +
    MVGaussian(
      mu    = c(-5, -5),
      sigma = matrix(c(1, 0, 0, 1), 2, 2)
    ) +
    MVGaussian(
      mu    = c( 5,  5),
      sigma = matrix(c(1, 0, 0, 1), 2, 2)
    ),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Multivariate Gaussian"
)

mySim <- sim(mySpec, T = 500, chain = 1, nSimulations = 200, seed = 9000)
y     <- extract_ysim(mySim, n = 1)

plot(y)
