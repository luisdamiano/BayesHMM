error_on_write_model <- function(spec) {
  tryCatch({
    write_model(spec, noLogLike = FALSE, writeDir = tempdir())
    write_model(spec, noLogLike = TRUE , writeDir = tempdir())
    TRUE
  }, error = function(e) {
    FALSE
  })
}

test_univariate_gaussian <- function() {
  mySpec <- hmm(
    K = 3, R = 1,
    observation = Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
    )
  )

  checkTrue(
    error_on_write_model(mySpec),
    "Can create Stan code for an observation model with univariate Gaussian density."
  )
}
