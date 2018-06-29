test_calibration_hmm_gaussian <- function() {
  N <- 2
  K <- 3
  R <- 1
  T <- 1000
  seed <- 9001

  set.seed(seed)

  mySpec <- hmm(
    K = K, R = R,
    observation = Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
    initial     = Dirichlet(alpha = rep(1, K)),
    transition  = Dirichlet(alpha = rep(1, K)),
    name = "test_calibration_hmm_gaussian"
  )

  mySim  <- sim(mySpec, T, chains = 1, iter = 500, seed = seed)
  y      <- do.call(c, extract(mySim, pars = "ypred"))

  myRes  <- sapply(1:N, function(n) {
    myFit  <- fit(mySpec, y = y, chains = 1, iter = 500, seed = seed + n)

    parTrue <- extract_obs(mySim)
    parFit  <- extract_obs(myFit)

    l <- lapply(1:length(parTrue), function(i) {
      ks.test(parTrue[[i]], parFit[[i]])
    })

    names(l) <- names(parTrue)
    rm(parTrue, parFit); gc();
    l
  })
}

# test_calibration_hmm_gaussian <- function() {
#   set.seed(9001)
#   N <- 2
#
#
#
#
#   calibrationDf <- sapply(1:N, function(n) {
#     parSim   <- sim_hmm_parameters(TLambda = 500, KLambda = 3)
#     muSim    <- cumsum(runif(4, 1, 10)) # 10 * rnorm(parSim$K, 0, 1)
#     sigmaSim <- 0.1
#     obsSim   <- function(z) {
#       as.matrix(rnorm(n = length(z), mean = muSim[z], sd = sigmaSim))
#     }
#     dataSim <- do.call(hmm_sim, c(parSim, list(obsSim = obsSim)))
#
#     mySpec <- hmm(
#       K = parSim$K, R = NCOL(dataSim$y),
#       observation = Gaussian(
#         mu    = Gaussian(0, 100),
#         sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
#       ),
#       initial     = Dirichlet(alpha = rep(1, parSim$K)),
#       transition  = Dirichlet(alpha = rep(1, parSim$K)),
#       name = "test_calibration_hmm_gaussian"
#     )
#
#     myFit <- fit(
#       mySpec, y = dataSim$y, chains = 1, iter = 500, seed = 9001
#     )
#
#     parFit  <- extract_obs(myFit)
#     parTrue <- do.call(c, lapply(1:parSim$K, function(i) { c(muSim[i], sigmaSim) }))
#     ranks   <- sapply(
#       1:length(parFit),
#       function(i) { sum(parFit[[i]] < parTrue[i]) / length(parFit[[i]]) }
#     )
#
#     convergence_diagnostics(myFit)
#     check_coverage(parTrue, parFit)
#   })
# }
