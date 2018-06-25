test_calibration_hmm_gaussian <- function() {
  set.seed(9001)

  parSim  <- sim_hmm_parameters(TLambda = 500, KLambda = 3)
  obsSim  <- function(z) {
    as.matrix(rnorm(n = length(z), mean = z, sd = 0.1))
  }
  dataSim <- do.call(hmm_sim, c(parSim, list(obsSim = obsSim)))

  mySpec <- hmm(
    K = parSim$K, R = NCOL(dataSim$y),
    observation = Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
    ),
    initial     = Default(),
    transition  = Default(),
    name = make_names(match.call())
  )

  myFit <- fit(
    mySpec, y = dataSim$y, chains = 1, iter = 500
  )

  parTrue <- c(unique(dataSim$z), rep(0.1, parSim$K))
  parFit  <- sapply(
    extract_obs(myFit),
    function(x) { quantile(x, probs = c(0.1, 0.9)) }
  )

  coverage_diagnostics(parTrue, parFit)
  convergence_diagnostics(myFit)
}
