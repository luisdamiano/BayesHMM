source(
  file.path(
    system.file(
      "calibration",
      package = "BayesHMM"
    ),
    "diag.R"
  )
)

test_calibration_hmm_gaussian <- function() {
  K <- 3
  R <- 1
  mySpec <- hmm(
    K = K, R = R,
    observation = Gaussian(
      mu    = Gaussian(0, 10),
      sigma = 1
    ),
    initial     = Dirichlet(alpha = rep(1, K)),
    transition  = Dirichlet(alpha = rep(1, K)),
    name = "test_calibration_hmm_gaussian"
  )

  diagnose_calibration(mySpec, N = 2, T = 500, iter = 500, seed = 9000)
}
