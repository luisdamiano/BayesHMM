explain         <- function(x, ...) { UseMethod("explain", x) }
generated       <- function(x, ...) { UseMethod("generated", x) }
getParameters   <- function(x, ...) { UseMethod("getParameters", x) }
is.multivariate <- function(x, ...) { UseMethod("is.multivariate", x) }
loglike         <- function(x, ...) { UseMethod("loglike", x) }
parameters      <- function(x, ...) { UseMethod("parameters", x) }
prior           <- function(x, ...) { UseMethod("prior", x) }

Density <- function(name, ...) {
  # Evaluate nested expressions (Densities)
  dots <- list(...)[[1]]
  for (i in seq_along(dots)) {
    if (is.language(dots[[i]])) {
      dots[[i]] <- eval(dots[[i]])
    }
  }

  # Check generic parameters
  check_list(dots[["bounds"]], 2, "bounds")
  check_list(dots[["trunc"]], 2, "trunc")

  structure(
    c(list(name = name), dots),
    class = c(name, "Density")
  )
}

`+.Density` <- function(x, y) {
  if (is.Density(x))
    return(list(x, y))

  c(x, list(y))
}

is.Density <- function(x) {
  "Density" %in% class(x)
}

explain.Density <- function(x) {
  # We should find a general way to explain the Density.
  sprintf(
    "%s(%s)",
    x[1],
    paste(names(x[-1]), "=", x[-1], collapse = ", ")
  )
}
