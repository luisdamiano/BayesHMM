test_calibration_hmm_Gaussian <- function() {
  K <- 3
  R <- 1
  mySpec <- hmm(
    K = K, R = R,
    observation = Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(1, 0.1, bounds = list(0, NULL))
    ),
    initial     = Dirichlet(alpha = rep(1, K)),
    transition  = Dirichlet(alpha = rep(1, K)),
    name = "test_calibration_hmm_Gaussian"
  )

  diagnose_calibration(mySpec, N = 100, T = 500, iter = 500, seed = 9000)
}

test_calibration_hmm_MVGaussianCor <- function() {
  K <- 3
  R <- 2
  mySpec <- hmm(
    K = K, R = R,
    observation = MVGaussianCor(
      mu    = Gaussian(mu = 0, sigma = 100),
      L     = LKJCor(eta = 2)
    ),
    initial     = Dirichlet(alpha = rep(1, K)),
    transition  = Dirichlet(alpha = rep(1, K)),
    name = "test_calibration_hmm_MVGaussianCor"
  )

  diagnose_calibration(mySpec, N = 1, T = 500, iter = 500, seed = 9000)
}

test_calibration_hmm_Binomial <- function() {
  K <- 3
  R <- 1
  mySpec <- hmm(
    K = K, R = R,
    observation = Binomial(
      theta = Beta(alpha = 1, beta = 1, bounds = list(0, 1)),
      N     = 10
    ),
    initial     = Dirichlet(alpha = rep(1, K)),
    transition  = Dirichlet(alpha = rep(1, K)),
    name = "test_calibration_hmm_Binomial"
  )

  diagnose_calibration(mySpec, N = 1, T = 500, iter = 500, seed = 9000)
}

test_calibration_all <- function() {
  tests <- c(
    "test_calibration_hmm_Gaussian", "test_calibration_hmm_Binomial"
  )

  library(doParallel)
  cl <- makeCluster(parallel::detectCores() / 4)
  df <- foreach(t = tests) %dopar% call(t)
  stopCluster(cl)

  df
}
