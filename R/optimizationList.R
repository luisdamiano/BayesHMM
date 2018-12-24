#' Return the optimization object for the run with the hightest posterior log density.
#'
#' It doesn't do anything fancy but it's a neat quality-of-life feature :).
#'
#' @inherit extract_grid
#' @param stanoptimList An object returned by \code{\link{optimizing}} when called with `nRuns` greater than one and `keep = "all"`.
#' #'
#' @examples
extract_best <- function(stanoptimList) {
  UseMethod("extract_best")
}

#' @keywords internal
#' #'
extract_best.OptimizationList <- function(stanoptimList) {
  ind       <- extract_grid(stanoptimList, pars = "none")[1, "n"]
  stanoptim <- stanoptimList[[ind]]
  attrNames <- names(attributes(stanoptimList))

  for (name in attrNames[-1]) {
    attr(stanoptim, name) <- attr(stanoptimList, name)
  }

  if (stanoptim$return_code != 0)
    warning("All runs have a non-zero return code. I am returning the run with highest log-posterior density -- USE WITH CAUTION.")

  stanoptim
}

#' @keywords internal
#' @describeIn extract_grid For OptimizationList objects, which are returned by \code{\link{optimizing}} with \emph{keep} set to \emph{all}.
#' #'
#' @examples
extract_grid.OptimizationList <- function(stanoptim, pars = NULL) {
  mat <- do.call(
    rbind,
    lapply(stanoptim, extract_grid, pars)
  )
  cbind(n = 1:NROW(mat), mat)[order(abs(mat[, "returnCode"]), -mat[, "logPosterior"]), ]
}
