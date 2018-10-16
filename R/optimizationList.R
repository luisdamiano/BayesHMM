#' Return the optimization object for the run with the hightest posterior log density.
#'
#' @inherit extract_grid
#' @param stanoptimList An object returned by \code{\link{optimizing}} when called with `nRuns` greater than one and `keep = "all"`.
#' @export
#' @examples
extract_best     <- function(x, ...) { UseMethod("extract_best", x) }

extract_best.OptimizationList <- function(stanoptimList, pars = NULL) {
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

#' Extract summary results from more than one run of the optimization procedure.
#'
#' @aliases extract_grid
#' @keywords internal
#' @inherit extract_grid
#' @param stanoptim An object returned by \code{\link{optimizing}}.
#' @export
#' @examples
extract_grid.OptimizationList <- function(stanoptim, pars = NULL) {
  mat <- do.call(
    rbind,
    lapply(stanoptim, extract_grid, pars)
  )
  cbind(n = 1:NROW(mat), mat)[order(abs(mat[, "returnCode"]), -mat[, "logPosterior"]), ]
}
