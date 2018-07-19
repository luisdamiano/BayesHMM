extract_grid     <- function(x, ...) { UseMethod("extract_grid", x) }

extract_grid.Optimization <- function(stanoptim, pars = NULL) {
  if (is.null(pars)) { pars <- "" }

  unlist(
    c(
      logPosterior = stanoptim$value,
      returnCode   = stanoptim$return_code,
      extract_time(stanoptim)[1:3],
      extract_quantity(stanoptim, pars = pars)
    )
  )
}

plot.Optimization <- function(stanoptim, pars, ...) {
  # plot(extract_quantity(stanoptim, pars))
  dotchart(
    x = rev(extract_quantity(stanoptim, pars, combine = c)), ...
  )
}

print.Optimization <- function(stanoptim, pars = NULL) {
  print(extract_quantity(stanoptim, pars))
}

is.stanoptim <- function(x) {
  is.list(x) & all(c("par", "value", "return_code") %in% names(x))
}
