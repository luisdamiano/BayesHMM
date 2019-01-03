# Internal utility functions with general purpose -------------------------
# This file contains internal utility functions.
# We regret to say that these functions are currently undocumented :(.

# Math & data types -------------------------------------------------------

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
    all(abs(x[lower.tri(x)]) < .Machine$double.eps) &
    all(diag(x) > 0)

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

check_list <- function(x, len, name = deparse(substitute(x))) {
  if (!is.list(x) || length(x) != len)
    stop(sprintf("%s must be a list with %s elements.", name, len))

  TRUE
}


# Text printouts ----------------------------------------------------------

#' Make a character string with a line (horizontal rule).
#'
#' This function creates a horizontal line using the character and text width set in the \emph{char} and \emph{textWidth} theme fields respectively.
#' @return A character string with a line.
#' @keywords internal
make_text_line <- function() {
  theme      <- getOption("BayesHMM.print")
  char       <- theme$char
  textWidth  <- theme$textWidth
  paste(rep(char, textWidth), collapse = "")
}

#' Make a character string with a header.
#'
#' This function formats the text into a header.
#' @return A character string with a header.
#' @keywords internal
make_text_header <- function(text) {
  textLine   <- make_text_line()

  sprintf(
    "%s\n%-80s\n%s\n",
    textLine, toupper(text), textLine
  )
}

#' Make a character string with a subheader.
#'
#' This function formats the text into a subheader.
#' @return A character string with a subheader.
#' @keywords internal
make_text_subheader <- function(text) {
  textLine   <- make_text_line()

  sprintf(
    "%s\n%s\n",
    textLine, text
  )
}

# Parser and writer -------------------------------------------------------

wrap     <- function(string, width = 80, ...) {
  collapse(strwrap(string, width, ...))
}

clean    <- function(string) {
  gsub("\n{2,}", "\n\n", string)
}

collapse <- function(...) {
  paste(..., sep = "\n", collapse = "\n")
}

split_return_line <- function(x) {
  if (grepl("\n", x)) {
    strsplit(x, "\n")[[1]]
  } else {
    x
  }
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

get_k <- function(x, paramName) {
  if (!is.Density(x[[paramName]]) || is.null(x[[paramName]]$k))
    return(x$k)

  x[[paramName]]$k
}

get_r <- function(x, paramName) {
  if (!is.Density(x[[paramName]]) || is.null(x[[paramName]]$r))
    return(x$r)

  x[[paramName]]$r
}

make_names <- function(s) {
  if (s == "")
    s <- strftime(Sys.time(), "%F %T")
  substr(gsub('[^a-zA-Z0-9]', '', make.names(s)), 1, 48)
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

make_ordered <- function(density, unordered, ordered) {
  if ( is.ordered.Density(density) ) { ordered } else { unordered }
}

is.empty <- function(x) {
  is.null(x) | length(x) == 0
}

# Thanks to dww: https://stackoverflow.com/a/38088874/2860744
is.scalar <- function(x) {
  is.atomic(x) && length(x) == 1L && !is.character(x) && Im(x)==0
}

numeric_to_stan <- function(x) {
  ret <- if(is.scalar(x)) {
    scalar_to_stan(x)
  } else if (is.vector(x)) {
    vector_to_stan(x)
  } else if (is.matrix(x)) {
    matrix_to_stan(x)
  } else {
    stop("Not a valid numeric.")
  }

  ret
}

scalar_to_stan <- function(x) {
  sprintf("%s", x)
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

cast_to_matrix <- function(x, nRow, nCol, name = deparse(substitute(x))) {
  if (is.matrix(x) && nrow(x) == nRow && ncol(x) == nCol)
    return(x)

  if (is.null(x))
    stop(
      sprintf(
        "%s is not a valid input because it is NULL. See ?make_data.",
        name
      ))

  xVector <- as.vector(x)
  if (length(xVector) != nRow * nCol)
    stop(
      sprintf(
        "Attempted to cast %s to a %dx%d matrix. The vector must have %d elements but %d were given. See ?make_data.",
        name, nRow, nCol, nRow*nCol, length(xVector)
      ))

  if (!is.numeric(xVector))
    stop(
      sprintf(
        "%s is not a valid input because it has non numeric elements. See ?make_data.",
        name
      )
    )

  matrix(as.numeric(xVector), nrow = nRow, ncol = nCol, byrow = FALSE)
}

cast_to_vector <- function(x, length, name = deparse(substitute(x))) {
  drop(cast_to_matrix(x = x, nRow = 1, nCol = length, name = name))
}

findGeneric <- function(fname, envir) {
  f <- function(pkg, name) {
    pkg  <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    get(name, envir = asNamespace(pkg), inherits = FALSE)
  }

  f("utils", "findGeneric")(fname, envir)
}

funinvarName <- function(x) {
  if (is.function(x)) {
    x <- deparse(substitute(x))
  }
  findGeneric(x, parent.frame())
}

get_dim <- function(x) {
  if (is.null(dim(x))) {
    length(x)
  } else {
    dim(x)
  }
}

# Posterior predictive checks ---------------------------------------------

prank <- function(x, y, ...) {
  check_scalar(x)
  check_vector(y)

  as.numeric(rank(c(x, y), ...)[1] / (length(y) + 1))
}

#' Return a function that computes the posterior intervals.
#'
#' @param ... Arguments to be passed to \code{\link{quantile}}.
#' @return A scalar with same type as `x`.
#' @examples
#' f <- posterior_intervals(probs = c(0.05, 0.95))
#' print(f(1:100))
posterior_intervals <- function(...) {
  function(x) {
    quantile(x, ...)
  }
}

#' Return the posterior mode of a vector.
#'
#' @param x A vector
#' @return A scalar with same type as `x`.
#' @examples posterior_mode(sample(1:3, 20, replace = TRUE))
posterior_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Print and text ----------------------------------------------------------

#' Remove non-alphabetic characters and modify to lowercase.
#'
#' @param string A character string, or a vector of character strings
#' @return A character string, or a vector of character strings.
#' @keywords internal
string_simplify <- function(string) {
  gsub("[^A-Za-z0-9]", "", tolower(string))
}

toproper <- function(string) {
  # Credits to Matthew Plourde https://stackoverflow.com/a/24957143/2860744
  gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(string), perl = TRUE)
}

get_time_info    <- function() {
  t <- Sys.time()
  sprintf("Printed on %s at %s.", format(t, "%Y-%m-%d"), format(t, "%T"))
}

get_platform_info <- function() {
  session     <- utils::sessionInfo()
  strPlatform <- sprintf("%s %s", session$running, session$platform)

  sprintf("%s", strPlatform)
}

get_R_info <- function() {
  session     <- utils::sessionInfo()
  strR        <- session$R.version$version.string

  sprintf("%s", strR)
}

get_other_packages_info <- function() {
  # Based on utils:::print.sessionInfo
  make_label <- function(L, n) {
    pkg  <- sapply(L[[n]], function(x) x[["Package"]])
    vers <- sapply(L[[n]], function(x) x[["Version"]])
    paste(pkg, vers, sep = " ")
  }

  session     <- utils::sessionInfo()
  paste(make_label(session, "otherPkgs"), collapse = ", ")
}

get_package_info <- function() {
  sprintf(
    "%s v%s (Build %s)",
    utils::packageDescription("BayesHMM")$Package,
    utils::packageDescription("BayesHMM")$Version,
    utils::packageDescription("BayesHMM")$Built
  )
}

get_rstan_info <- function() {
  sprintf(
    "%s v%s (Build %s)",
    utils::packageDescription("rstan")$Package,
    utils::packageDescription("rstan")$Version,
    utils::packageDescription("rstan")$Built
  )
}

# Plots -------------------------------------------------------------------

col2rgb_alpha <- function(bgCol, alpha = 1) {
  if (alpha >= 0 && alpha <= 1) {
    alpha <- alpha * 255
  } else if (alpha < 0 || alpha > 255) {
    stop("Not a valid entry for alpha (transparency).")
  }

  apply(
    col2rgb(bgCol, alpha = FALSE),
    2,
    function(tidx) {
      rgb(tidx[1], tidx[2], tidx[3], alpha = alpha, maxColorValue = 255)
    }
  )
}

par_reset <- function() {
  invisible(tryCatch({dev.off()}, error = function(e) { }))
}

get_ytop    <- function() { par()$usr[4] }

get_ybottom <- function() { par()$usr[3] }

par_edit    <- function(par, ...) {
  dots  <- list(...)
  for (i in seq_len(length(dots))) {
    if (!is.null(dots[[i]])) {
      par[i] <- dots[[i]]
    }
  }
  par
}
