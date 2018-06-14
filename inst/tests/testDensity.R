test_univariate_gaussian <- function() {
  mySpec <- hmm(
    K = 3, R = 1,
    observation = Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
    ),
    initial     = Dirichlet(alpha = c(1, 1, 1)),
    transition  = Dirichlet(alpha = c(1, 1, 1))
  )

  checkTrue(
    error_in_write_model(mySpec),
    "Can create Stan code for an observation model with univariate Gaussian density."
  )
}

