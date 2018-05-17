makeNames <- function(s) {
  substr(gsub('[^a-zA-Z]', '', make.names(s)), 1, 32)
}

collapse <- function(...) {
  paste(..., sep = "\n", collapse = "\n")
}

simpleApply <- function(X, FUN, func1 = identity, ...) {
  lapply(func1(X), FUN, ...)
}

doubleApply <- function(X, FUN, func1 = identity, func2 = identity, ...) {
  simpleApply(X, function(subX) {
    collapse(
      simpleApply(subX, FUN, func1 = func1)
    )
  }, func1 = func2)
}

tripleApply <- function(X, FUN, func1 = identity, func2 = identity, func3 = identity, ...) {
  simpleApply(X, function(subX) {
    collapse(
      doubleApply(subX, FUN, func1 = func1, func2 = func2)
    )
  }, func1 = func3)
}
