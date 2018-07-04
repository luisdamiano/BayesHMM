.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf(
      "Hey! BayesHHM v%s here o/",
      utils::packageDescription("BayesHMM")$Version
    )
  )

  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  opNew <- list(
    BayesHHM.config = "example",
    BayesHMM.colors.clusters = c(
      "#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF",
      "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"
      ),
    BayesHMM.colors.yLine = "gray40",
    BayesHMM.colors.shadeAlpha = 0.2
  )
  toset <- !(names(opNew) %in% names(op))
  if (any(toset)) options(opNew[toset])

  invisible()
}
