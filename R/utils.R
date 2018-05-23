make_names <- function(s) {
  substr(gsub('[^a-zA-Z1-9]', '', make.names(s)), 1, 48)
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

check_list <- function(x, len, name) {
  if (!is.list(x) || length(x) != len)
    stop(
      sprintf("%s must be a list with %s elements.", name, len)
    )
}

check_whole <- function(x, name) {
  if (length(x) != 1 || !is.numeric(x) || !(abs(x - round(x)) < .Machine$double.eps^0.5))
    stop(
      sprintf("%s must be an integer.", name)
    )
}

make_limit  <- function(x, name, limit, s1, s2, s3) {
  nameLimit <- paste0(name, limit)
  s <- ""
  if (!is.null(x[[nameLimit]][[1]]) & !is.null(x[[nameLimit]][[2]])) {
    s <- sprintf(s1, x[[nameLimit]][[1]], x[[nameLimit]][[2]])
  } else {
    if (!is.null(x[[nameLimit]][[1]]))
      s <- sprintf(s2, x[[nameLimit]][[1]])

    if (!is.null(x[[nameLimit]][[2]]))
      s <- sprintf(s3, x[[nameLimit]][[2]])
  }
  return(s)
}

make_bounds <- function(x, name) {
  make_limit(
    x, name, "Bounds", "<lower = %s, upper = %s>", "<lower = %s>", "<upper = %s>"
  )
}

make_trunc <- function(x, name) {
  make_limit(x, name, "trunc", "T[%s, %s]", "T[%s, ]", "T[, %s]")
}

make_subindex <- function(x) {
  s <- if (x$k == x$r & x$r == "") {
    ""
  } else if (x$k != "" & x$r == "") {
    sprintf("[%s]", x$k)
  } else {
    sprintf("[%s, %s]", x$k, x$r)
  }
  return(s)
}
