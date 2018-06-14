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

check_scalar <- function(x) {
  is.atomic(x) & is.numeric(x) & length(x) == 1L
}

check_vector <- function(x) {
  is.vector(x) & !is.list(x) & all(sapply(x, is.numeric)) &
    !any(sapply(x, is.na))
}

check_simplex <- function(x) {
  check_vector(x) & sum(x) == 1 & min(x) >= 0
}

check_matrix <- function(x) {
  isTRUE(is.matrix(x))
}

check_cov_matrix <- function(x) {
  check_matrix(x) & check_psd(x)
}

check_cholesky_factor <- function(x) {
  # Requirements for a Cholesky factor for a cov matrix:
  # * is a matrix :P,
  # * lower triangular,
  # * positive diagonal
  check_matrix(x) &
    all(abs(upper.tri(x)) < .Machine$double.eps) &
    all(diag(x)) > 0
}

check_cholesky_factor_cov <- function(x) {
  check_cholesky_factor(x) & check_psd(x %*% t(x))
}

check_cholesky_factor_cor <- function(x) {
  check_cholesky_factor(x) &
    check_simplex(diag(x))
}

# check_vector <- function(x, len) {
#   is.vector(x) & !is.list(x) & all(sapply(x, is.numeric)) &
#   !any(sapply(x, is.na)) & length(x) == len
# }
#
# check_matrix <- function(x, nrow, ncol) {
#   isTRUE(is.matrix(x) & nrow(x) == nrow & ncol(x) == ncol)
# }

check_list <- function(x, len, name) {
  if (!is.list(x) || length(x) != len)
    stop(
      sprintf("%s must be a list with %s elements.", name, len)
    )
}

check_whole <- function(x, name) {
  if (length(x) != 1
      || !is.numeric(x)
      || !(abs(x - round(x)) < .Machine$double.eps^0.5))
    stop(
      sprintf("%s must be an integer.", name)
    )
}

collapse <- function(...) {
  paste(..., sep = "\n", collapse = "\n")
}

# densityApply <- function(X, FUN, ..., simplify = TRUE) {
#   if (is.Density(X)) {
#     FUN(X, ...)
#   # } else if (any(sapply(X, is.Density))) {
#   } else {
#     sapply(X, function(x) {
#       densityApply(x, FUN, ..., simplify = simplify)
#     })
#   }
# }

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
  # both the parent distribution density (x$multivariate)
  # and the prior distribution density (is.multivariate(x))
  # sprintf(if (x$multivariate & is.multivariate(x)) { "[%s]" } else { "%s" }, x$r)
  sprintf(if (x$multivariate) { "[%s]" } else { "%s" }, x$r)
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
  is.null(x) |  length(x) == 0
}

is.freeParameter <- function(x) {
  is.Density(x) |
    is.list(x) & all(sapply(x, is.Density))
}

