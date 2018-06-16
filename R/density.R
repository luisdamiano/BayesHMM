data               <- function(x, ...) { UseMethod("data", x) }
explain            <- function(x) { UseMethod("explain", x) }
constants          <- function(x) { UseMethod("constants", x) }
generated          <- function(x) { UseMethod("generated", x) }
getParameterNames  <- function(x) { UseMethod("getParameterNames", x) }
getFreeParameters  <- function(x) { UseMethod("getFreeParameters", x) }
getFixedParameters <- function(x) { UseMethod("getFixedParameters", x) }
is.discrete        <- function(x) { UseMethod("is.discrete", x) }
is.multivariate    <- function(x) { UseMethod("is.multivariate", x) }
logLike            <- function(x) { UseMethod("logLike", x) }
parameters         <- function(x) { UseMethod("parameters", x) }
freeParameters     <- function(x) { UseMethod("freeParameters", x) }
fixedParameters    <- function(x) { UseMethod("fixedParameters", x) }
prior              <- function(x) { UseMethod("prior", x) }
noLogLike          <- function(x) { UseMethod("noLogLike", x) }

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

MultivariateDensity <- function(name, ...) {
  x <- Density(name, ...)
  class(x) <- append(class(x), "MultivariateDensity", 1)
  x
}

DiscreteDensity <- function(name, ...) {
  x <- Density(name, ...)
  class(x) <- append(class(x), "DiscreteDensity", 1)
  x
}

MultivariateDiscreteDensity <- function(name, ...) {
  x <- DiscreteDensity(name, ...)
  class(x) <- append(class(x), "MultivariateDensity", 1)
  x
}

PriorOnlyDensity <- function(name, ...) {
  x <- Density(name, ...)
  class(x) <- append(class(x), "PriorOnlyDensity", 1)
  x
}

PriorOnlyMultivariateDensity <- function(name, ...) {
  x <- MultivariateDensity(name, ...)
  class(x) <- append(class(x), "PriorOnlyDensity", 1)
  x
}

`+.Density` <- function(x, y) {
  if (is.Density(x))
    return(list(x, y))

  c(x, list(y))
}

is.Density <- function(x) {
  "Density" %in% class(x)
}

data.Density <- function(x, noLogLike) {
  "matrix[T, R] y;  // observations"
}

data.DiscreteDensity <- function(x, noLogLike) {
  "int y[T, R];     // observations"
}

explain.Density <- function(x) {
  # We should find a general way to explain the Density.
  sprintf(
    "%s(%s)",
    x[1],
    paste(names(x[-1]), "=", x[-1], collapse = ", ")
  )
}

getFreeParameters.Density <- function(x) {
  l <-
    sapply(
      getParameterNames(x),
      function(paramName) {
        if (is.Density(x[[paramName]])) {
          eval(x[[paramName]])
        }
      },
      simplify = FALSE
    )

  l[!sapply(l, is.null)]
}

getFixedParameters.Density <- function(x) {
  l <-
    sapply(
      getParameterNames(x),
      function(paramName) {
        if (!is.Density(x[[paramName]])) {
          x[[paramName]]
        }
      },
      simplify = FALSE
    )

  l[!sapply(l, is.null)]
}

noLogLike.Density <- function(x) {
  sprintf("loglike[%s][t] = 1;", x$k)
}

is.multivariate.Density <- function(x) { FALSE }
is.multivariate.MultivariateDensity <- function(x) { TRUE }

is.discrete.Density <- function(x) { FALSE }
is.discrete.DiscreteDensity <- function(x) { TRUE }

constants.Density <- function(x) { "" }

freeParameters.PriorOnlyDensity <- function(x) {
  stop("This density is only meant to be used as a prior.")
}

fixedParameters.PriorOnlyDensity <- function(x) {
  stop("This density is only meant to be used as a prior.")
}

generated.PriorOnlyDensity <- function(x) {
  stop("This density is only meant to be used as a prior.")
}

getFreeParameters.PriorOnlyDensity <- function(x) {
  stop("This density is only meant to be used as a prior.")
}

logLike.PriorOnlyDensity <- function(x) {
  stop("This density is only meant to be used as a prior.")
}
