#' Prologue
#'
#' @keywords internals
#' @name zzz
#' @importClassesFrom rstan stanfit
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf(
      "Hola! BayesHHM v%s here o/",
      utils::packageDescription("BayesHMM")$Version
    )
  )

  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
}

.onLoad <- function(libname, pkgname) {
  # See
  # 1. https://github.com/stan-dev/rstan/issues/176
  # 2. https://github.com/stan-dev/rstan/issues/353
  # 3. https://github.com/krisrs1128/nmfSim/commit/225ef3970c04d7e4a3e477ed2be7a02bf945eb43
  # 4. https://stackoverflow.com/a/6279889/2860744
  # suppressPackageStartupMessages(library(rstan))

  opDefaults <- list(
    BayesHHM.config = "example",
    BayesHMM.print = list(
      char         = "_",
      tab          = "  ",
      textWidth    = 80
    ),
    BayesHMM.theme = list(
      states       = c(
        "#E41F26", "#2EA147", "#1D79B4",
        # "#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF",
        "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"
      ),
      observations = "black",
      yDensity     = "black",
      yPredDensity = "gray80",
      yLine        = "gray40",
      boxY         = "black",
      boxYPred     = "gray80",
      histCol      = "gray80",
      histBorder   = "gray",
      histLine     = "black",
      shadeAlpha   = 0.2
    )
  )

  op    <- options()
  toset <- !(names(opDefaults) %in% names(op))
  if (any(toset)) options(opDefaults[toset])

  invisible()
}
