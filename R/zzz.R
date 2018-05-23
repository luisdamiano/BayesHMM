.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Hi! BayesHHM v0.1 here o/")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    BayesHHM.config = "example"
  )
  toset <- !(names(op.devtools) %in% names(op))
  if (any(toset)) options(op.devtools[toset])

  invisible()
}
