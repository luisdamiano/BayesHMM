test_fit <- function() {
  seed   <- 9000
  mySpec <- hmm(
    K = 2, R = 1,
    observation = Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
    ),
    initial     = Dirichlet(alpha = c(1, 1)),
    transition  = Dirichlet(alpha = c(1, 1)),
    name = "Univariate Gaussian Hidden Markov Model for unit tests"
  )

  DEACTIVATED("Temporarily deactivated (too slow).")

  no_error_in_expr({
    # Simulation
    myModel <- compile(mySpec)
    mySim0  <- sim(mySpec , T = 100, nSimulations = 1, seed = seed, chain = 1)
    y       <- extract_ysim(mySim0)

    # From compiled model
    myDraw1 <- draw_samples(myModel, y, iter = 400, chain = 1)
    myFit1  <- fit(myModel, y, iter = 400, chain = 1)
    myOpt1  <- optimizing(myModel, y, nRuns = 1)
    mySim1  <- sim(myModel, T = 100, nSimulations = 1, chain = 1, seed = seed)

    # From fitted object
    myDraw2 <- draw_samples(myFit1, y, iter = 400, chain = 1)
    myFit2  <- fit(myFit1, y, iter = 400, chain = 1)
    myOpt2  <- optimizing(myFit1, y, nRuns = 1)
    mySim2  <- sim(myFit1, T = 100, nSimulations = 1, chain = 1, seed = seed)

    # From Optimization
    myDraw3 <- draw_samples(myOpt1, y, iter = 400, chain = 1)
    myFit3  <- fit(myOpt1, y, iter = 400, chain = 1)
    myOpt3  <- optimizing(myOpt1, y, nRuns = 1)
    mySim3  <- sim(myOpt1, T = 100, nSimulations = 1, chain = 1, seed = seed)

    # From Specification
    myDraw4 <- draw_samples(mySpec, y, iter = 400, chain = 1)
    myFit4  <- fit(mySpec, y, iter = 400, chain = 1)
    myOpt4  <- optimizing(mySpec, y, nRuns = 1)
  })
}
