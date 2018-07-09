test_calibration_dummy_model <- function() {
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

  diagnose_calibration(mySpec, N = 3, T = 500, iter = 500, seed = 9000)
}

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

test_calibration_hmm_Binomial <- function() {
  K <- 3
  R <- 1

  mySpec <- hmm(
    K = K, R = R,
    observation =
      Binomial(
        theta = Beta(alpha = 0.2, beta = 1, bounds = list(0, 1)),
        N     = 10
      ) +
      Binomial(
        theta = Beta(alpha = 100, beta = 100, bounds = list(0, 1)),
        N     = 10
      ) +
      Binomial(
        theta = Beta(alpha = 1, beta = 0.2, bounds = list(0, 1)),
        N     = 10
      )
    ,
    initial     = Dirichlet(alpha = rep(1, K)),
    transition  = Dirichlet(alpha = rep(1, K)),
    name = "test_calibration_hmm_Binomial"
  )

  diagnose_calibration(mySpec, N = 100, T = 500, iter = 500, seed = 9000)
}

test_calibration_hmm_RegGaussian <- function() {
  K <- 3
  R <- 1
  mySpec <- hmm(
    K = K, R = R,
    observation = RegGaussian(
      xBeta = Gaussian(0, 10),
      sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL)),
      M     = 3
    ),
    initial     = Dirichlet(alpha = rep(1, K)),
    transition  = Dirichlet(alpha = rep(1, K)),
    name = "test_calibration_hmm_RegGaussian"
  )

  set.seed(9000)
  x <- as.matrix(
    cbind(
      rep(1, 500),
      rnorm(500),
      rnorm(500)
    )
  )

  diagnose_calibration(mySpec, N = 100, T = 500, iter = 500, seed = 9000, x)
}

test_calibration_hmm_MVGaussianCor <- function() {
  K <- 3
  R <- 2
  mySpec <- hmm(
    K = K, R = R,
    observation = MVGaussianCor(
      mu    = Gaussian(mu = 0, sigma = 10),
      L     = Default()
      # mu    = Gaussian(mu = 0, sigma = 100),
      # L     = LKJCor(eta = 2)
    ),
    initial     = Dirichlet(alpha = rep(1, K)),
    transition  = Dirichlet(alpha = rep(1, K)),
    name = "test_calibration_hmm_MVGaussianCor"
  )

  diagnose_calibration(mySpec, N = 1, T = 500, iter = 500, seed = 9000)
}

test_calibration_all <- function() {
  tests <- c(
    "test_calibration_hmm_Gaussian", "test_calibration_hmm_Binomial"
  )

  do.call(rbind, lapply(tests, function(test) { eval(call(test)) }))
}
