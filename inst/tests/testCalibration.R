test_calibration_dummy_model <- function() {
  RUnit::DEACTIVATED("Calibration test is currently deactivated (too slow).")
  K <- 3
  R <- 1
  mySpec <- hmm(
    K = K, R = R,
    observation =
      Gaussian(mu = Gaussian(mu = -10, sigma = 1), sigma = 1) +
      Gaussian(mu = Gaussian(mu =   0, sigma = 1), sigma = 1) +
      Gaussian(mu = Gaussian(mu =  10, sigma = 1), sigma = 1),
    initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
    transition  =
      Dirichlet(alpha = c(1.0, 0.2, 0.2)) +
      Dirichlet(alpha = c(0.2, 1.0, 0.2)) +
      Dirichlet(alpha = c(0.2, 0.2, 1.0)),
    name = "Univariate Gaussian Dummy Model"
  )

  validate_calibration(mySpec, N = 3, T = 500, iter = 500, seed = 9000)
}
