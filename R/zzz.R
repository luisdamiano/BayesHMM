.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf(
      "Hey! BayesHHM v%s here o/",
      packageDescription("BayesHMM")$Version
    )
  )
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  opNew <- list(
    BayesHHM.config = "example"
  )
  toset <- !(names(opNew) %in% names(op))
  if (any(toset)) options(opNew[toset])

  invisible()
}
