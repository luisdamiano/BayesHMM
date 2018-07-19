check_psd <- function(x) {
  if (requireNamespace("matrixcalc", quietly = TRUE)) {
    matrixcalc::is.positive.semi.definite(x)
  } else {
    warnings(
      "You have set a fixed Cholesky factor for a covariance matrix. Because the package matrixcalc is not installed, we could not check if the fixed Cholesky factor is valid."
    )
    return(TRUE)
  }
}

check_scalar <- function(x, name = deparse(substitute(x))) {
  ok <- is.atomic(x) & is.numeric(x) & length(x) == 1L

  if (!ok) {
    stop(sprintf("%s must be a scalar (numeric element of lengh one).", name))
  }

  TRUE
}

check_vector <- function(x, name = deparse(substitute(x))) {
  ok <- is.vector(x) & !is.list(x) & all(sapply(x, is.numeric)) &
    !any(sapply(x, is.na))

  if (!ok) {
    stop(sprintf("%s must be a vector.", name))
  }

  TRUE
}

check_simplex <- function(x, name = deparse(substitute(x))) {
  ok <- check_vector(x) & identical(sum(x), 1) & min(x) >= 0

  if (!ok) {
    stop(sprintf("%s must be a simplex (vector with non-negative elements summing to 1.", name))
  }

  TRUE
}

check_matrix <- function(x, name = deparse(substitute(x))) {
  ok <- isTRUE(is.matrix(x))

  if (!ok) {
    stop(sprintf("%s must be a matrix.", name))
  }

  TRUE
}

check_transition_matrix <- function(x, name = deparse(substitute(x))) {
  ok <- isTRUE(is.matrix(x)) & dim(x)[1] == dim(x)[2] & min(x) >= 0 &
    all(apply(x, 1, function(xi) { identical(sum(xi), 1) } ))

  if (!ok) {
    stop(sprintf("%s must be a square matrix with simplex rows.", name))
  }

  TRUE
}

check_cov_matrix <- function(x, name = deparse(substitute(x))) {
  ok <- check_matrix(x) & check_psd(x)

  if (!ok) {
    stop(sprintf("%s must be a positive-semidefinite matrix.", name))
  }

  TRUE
}

check_cholesky_factor <- function(x, name = deparse(substitute(x))) {
  # Requirements for a Cholesky factor for a cov matrix:
  # * is a matrix :P,
  # * lower triangular,
  # * positive diagonal
  ok <- check_matrix(x) &
    all(abs(upper.tri(x)) < .Machine$double.eps) &
    all(diag(x)) > 0

  if (!ok) {
    stop(sprintf("%s must be a valid Cholesky factor (lower triangular matrix with positive elements in the diagonal).", name))
  }

  TRUE
}

check_cholesky_factor_cov <- function(x, name = deparse(substitute(x))) {
  ok <- check_cholesky_factor(x) & check_psd(x %*% t(x))

  if (!ok) {
    stop(sprintf("%s must be a valid Cholesky factor for a covariance matrix (lower triangular matrix with positive elements in the diagonal that is positive-semidefinite when multiplied by its transpose.).", name))
  }

  TRUE
}

check_cholesky_factor_cor <- function(x, name = deparse(substitute(x))) {
  ok <- check_cholesky_factor(x) & check_simplex(diag(x))

  if (!ok) {
    stop(sprintf("%s must be a valid Cholesky factor for a correlation matrix (lower triangular matrix with positive elements in the diagonal that has non-negative elements in the diagonals summing to one).", name))
  }

  TRUE
}

check_list <- function(x, len, name = deparse(substitute(x))) {
  if (!is.list(x) || length(x) != len)
    stop(sprintf("%s must be a list with %s elements.", name, len))

  TRUE
}

check_whole <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1
      || !is.numeric(x)
      || !(abs(x - round(x)) < .Machine$double.eps^0.5))
    stop(sprintf("%s must be an integer.", name))

  TRUE
}

check_natural <- function(x, name = deparse(substitute(x))) {
  check_whole(x, name)
  if (x < 1) {
    stop(sprintf("%s must be a positive integer (> 0).", name))
  }

  TRUE
}

collapse <- function(...) {
  paste(..., sep = "\n", collapse = "\n")
}

densityApply <- function(X, FUN, ..., simplify = TRUE) {
  if (is.Density(X)) {
    FUN(X)
  } else if (any(sapply(X, is.Density))) {
    sapply(X, FUN, ..., simplify = simplify)
  } else {
    l <- lapply(X, function(x) {
      sapply(x, FUN, ..., simplify = simplify)
    })

    do.call(c, l)
  }
}

densityCollect <- function(X, FUN, ..., simplify = TRUE) {
  collapse(unique(densityApply(X, FUN, ..., simplify = TRUE)))
}

make_names <- function(s) {
  substr(gsub('[^a-zA-Z1-9]', '', make.names(s)), 1, 48)
}

make_limit  <- function(x, name, limit, strLU, strL, strU) {
  nameLimit <- paste0(name, limit)
  s <- ""
  if (!is.null(x[[nameLimit]][[1]]) & !is.null(x[[nameLimit]][[2]])) {
    s <- sprintf(strLU, x[[nameLimit]][[1]], x[[nameLimit]][[2]])
  } else {
    if (!is.null(x[[nameLimit]][[1]]))
      s <- sprintf(strL, x[[nameLimit]][[1]])

    if (!is.null(x[[nameLimit]][[2]]))
      s <- sprintf(strU, x[[nameLimit]][[2]])
  }
  return(s)
}

make_bounds <- function(x, name) {
  make_limit(
    x, name, "Bounds", "<lower = %s, upper = %s>", "<lower = %s>", "<upper = %s>"
  )
}

make_trunc <- function(x, name) {
  make_limit(
    x, name, "trunc", "T[%s, %s]", "T[%s, ]", "T[, %s]"
  )
}

make_rsubindex <- function(x) {
  sprintf(if (x$multivariate) { "[%s]" } else { "%s" }, x$r)
}

make_parameters <- function(density, string, stringNot = "",
                            isDensity = TRUE, check = NULL, errorStr = "") {
  if (is.Density(density) == isDensity) {
    if (!is.null(check)) {
      if (!check(density)) { stop(errorStr) }
    }

    string
  } else {
    stringNot
  }
}

make_free_parameters <- function(density, string) {
  make_parameters(density, string, stringNot = "", isDensity = TRUE)
}

make_fixed_parameters <- function(density, string, check = NULL, errorStr = "") {
  make_parameters(density, string, stringNot = "", isDensity = FALSE, check, errorStr)
}

vector_to_stan <- function(x) {
  sprintf("[%s]", paste(x, collapse = ", "))
}

matrix_to_stan <- function(x) {
  colStr <- sprintf(
    "[%s]",
    apply(x, 1, function(x) { paste(x, collapse = ", ") })
  )

  sprintf(
    "[%s]",
    paste(colStr, collapse = ", ")
  )
}

is.empty <- function(x) {
  is.null(x) | length(x) == 0
}

is.freeParameter <- function(x) {
  is.Density(x) |
    is.list(x) & all(sapply(x, is.Density))
}

funinvarName <- function(x) {
  if (is.function(x)) {
    x <- deparse(substitute(x))
  }
  utils:::findGeneric(x, parent.frame())
}

get_dim <- function(x) {
  if (is.null(dim(x))) {
    length(x)
  } else {
    dim(x)
  }
}

prank <- function(x, y, ...) {
  check_scalar(x)
  check_vector(y)

  as.numeric(rank(c(x, y), ...)[1] / (length(y) + 1))
}

posterior_intervals <- function(...) {
  function(x) {
    quantile(x, ...)
  }
}

posterior_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
