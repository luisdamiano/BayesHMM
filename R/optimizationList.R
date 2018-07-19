extract_best     <- function(x, ...) { UseMethod("extract_best", x) }

extract_best.OptimizationList <- function(stanoptimList, pars = NULL) {
  # ind       <- which.max(sapply(stanoptimList, "[[", "value"))
  ind       <- extract_grid(stanoptimList, pars = "none")[1, "n"]
  stanoptim <- stanoptimList[[ind]]
  attrNames <- names(attributes(stanoptimList))

  for (name in attrNames[-1]) {
    attr(stanoptim, name) <- attr(stanoptimList, name)
  }

  if (myBest$return_code != 0)
    warning("All runs have a non-zero return code. I am returning the run with highest log-posterior density -- USE WITH CAUTION.")

  stanoptim
}

extract_grid.OptimizationList <- function(stanoptim, pars = NULL) {
  mat <- do.call(
    rbind,
    lapply(stanoptim, extract_grid, pars)
  )
  cbind(n = 1:NROW(mat), mat)[order(abs(mat[, "returnCode"]), -mat[, "logPosterior"]), ]
}
