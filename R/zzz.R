#' @details
#' See the Introduction vignette: vignette("introduction", package = "BayesHMM")
#' Additionally, you may start with the manual help for ?specify and ?fit.
#' @keywords internal
"_PACKAGE"

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

  # Create a global option with the current theme
  load_theme()
}

.onLoad <- function(libname, pkgname) {

}
