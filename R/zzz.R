#' Prologue
#'
#' @keywords internal
#' @name zzz
#' @importClassesFrom rstan stanfit
#' @import utils
NULL

# Hack to avoid R CMD check
# You may find what functions are affected by the hack simply by
# uncommenting the following line and running R CMD check.
utils::globalVariables(c("n"))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf(
      "Hola! BayesHHM v%s here o/",
      utils::packageDescription("BayesHMM")$Version
    )
  )

  # For integration with rstan, see
  # 1. https://github.com/stan-dev/rstan/issues/176
  # 2. https://github.com/stan-dev/rstan/issues/353
  # 3. https://github.com/krisrs1128/nmfSim/commit/225ef3970c04d7e4a3e477ed2be7a02bf945eb43
  # 4. https://stackoverflow.com/a/6279889/2860744
  # suppressPackageStartupMessages(library(rstan))
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
}

.onLoad <- function(libname, pkgname) {
  load_theme()
}
