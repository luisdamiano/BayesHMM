# Stan blocks & included chunks
constants          <- function(x) { UseMethod("constants", x) }
generated          <- function(x) { UseMethod("generated", x) }
parameters         <- function(x) { UseMethod("parameters", x) }
logLike            <- function(x) { UseMethod("logLike", x) }
noLogLike          <- function(x) { UseMethod("noLogLike", x) }
link               <- function(x) { UseMethod("link", x) }
freeParameters     <- function(x) { UseMethod("freeParameters", x) }
fixedParameters    <- function(x) { UseMethod("fixedParameters", x) }
prior              <- function(x) { UseMethod("prior", x) }

# Other (not directly related to Stan code)
explain            <- function(x) { UseMethod("explain", x) }
getParameterNames  <- function(x) { UseMethod("getParameterNames", x) }
getFreeParameters  <- function(x) { UseMethod("getFreeParameters", x) }
getFixedParameters <- function(x) { UseMethod("getFixedParameters", x) }
is.link            <- function(x) { UseMethod("is.link", x) }
is.discrete        <- function(x) { UseMethod("is.discrete", x) }
is.multivariate    <- function(x) { UseMethod("is.multivariate", x) }

#' Create a representation of a probability mass or density function. It can be
#' used to specify either a prior distribution for a model parameter or a
#' likelihood function for an observation model.
#'
#' @param name A character vector with the name of the density.
#' @param ...  Other arguments for the density.
#'
#' @return A Density object.
#' @family Density
Density <- function(name, ...) {
  # Evaluate nested expressions (Densities)
  dots <- list(...)[[1]]
  for (i in seq_along(dots)) {
    if (is.language(dots[[i]])) {
      dots[[i]] <- eval(dots[[i]])
    }
  }

  # Check for generic parameters
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

LinkDensity <- function(name, ...) {
  x <- Density(name, ...)
  class(x) <- append(class(x), "LinkDensity", 1)
  x
}

is.Density <- function(x) {
  "Density" %in% class(x)
}

block_data.Density <- function(x, noLogLike) {
  if (noLogLike) {
    "// No observation vector"
  } else {
    "matrix[T, R] y;  // observations"
  }
}

block_data.DiscreteDensity <- function(x, noLogLike) {
  if (noLogLike) {
    "// No observation vector"
  } else {
    "int y[T, R];     // observations"
  }
}

explain.Density <- function(x) {
  freeParam  <- getFreeParameters(x)
  fixedParam <- getFixedParameters(x)

  strBounds <- paste(
    if (is.null(x$bounds[[1]])) { "(-infty" } else { sprintf("[%s", x$bounds[[1]]) },
    if (is.null(x$bounds[[2]])) { "infty)"  } else { sprintf("%s]", x$bounds[[2]]) },
    sep = ", "
  )

  strTrunc  <- paste(
    if (is.null(x$trunc[[1]])) { "(-infty" } else { sprintf("[%s", x$trunc[[1]]) },
    if (is.null(x$trunc[[2]])) { "infty)"  } else { sprintf("%s]", x$trunc[[2]]) },
    sep = ", "
  )

  strFreeParam <-
    if (is.null(freeParam) || length(freeParam) == 0) {
      NULL
    } else {
      l <- lapply(names(freeParam), function(paramName) {
        e <- sprintf(
          "\n\t\t%-5s = %s",
          paramName, explain(freeParam[[paramName]])
        )

        e <- gsub("\t", "\t\t\t", e)
        e <- gsub("^\n\t\t\t\t\t\t", "\n\t\t", e)
        e
      })

      paste(l, collapse = ", ")
    }

  strFixedParam <-
    if (is.null(fixedParam) || length(fixedParam) == 0) {
      NULL
    } else {
      l <- lapply(names(fixedParam), function(paramName) {
        sprintf(
          "%s = %s",
          paramName, paste(fixedParam[[paramName]], collapse = "")
        )
      })

      paste(l, sep = "", collapse = ", ")
    }

  block1 <-
    sprintf(
      "%sDensity: %s %s",
      if (is.null(x$param)) { "Variable " } else { "Prior " },
      x$name,
      if (all(sapply(x$trunc, is.null))) { strBounds } else { strTrunc }
    )

  block2 <-
    if (!is.null(freeParam) && length(freeParam) != 0)
      sprintf(
        "\tFree parameters: %d (%s)%s",
        length(freeParam),
        paste(names(freeParam), collapse = ", "),
        strFreeParam
      )

  block3 <-
    if (!is.null(fixedParam) && length(fixedParam) != 0)
      sprintf(
        "\tFixed parameters: %d (%s)",
        length(fixedParam),
        strFixedParam
      )

  collapse(c(block1, block2, block3))
}

getParameterNames.Density <- function(x) {
  warning(
    sprintf(
      "getParameterNames not implemented for the %s density.",
      x$name
    )
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

  l <- l[!sapply(l, is.null)]

  if (is.empty(l)) {
    return(NULL)
  } else {
    dl <-
      if (length(l) == 1 ) {
        `+.Density`(l)
      } else {
        Reduce(`+.Density`, l)
      }
    names(dl) <- names(l)
    return(dl)
  }
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

is.link.Density <- function(x) { FALSE }
is.link.LinkDensity <- function(x) { TRUE }

constants.Density <- function(x) { "" }

freeParameters.PriorOnlyDensity <- function(x) {
  ""
}

fixedParameters.PriorOnlyDensity <- function(x) {
  ""
}

generated.PriorOnlyDensity <- function(x) {
  ""
}

getFreeParameters.PriorOnlyDensity <- function(x) {
  ""
}

logLike.PriorOnlyDensity <- function(x) {
  ""
}

link.Density <- function(x) { "" }

is.TVInitial.Density <- function(x) { FALSE }
is.TVTransition.Density <- function(x) { FALSE }

#' Append two Density objects.
#'
#' @aliases +
#' @usage x + y
#' @param x A Density object (e.g. \code{\link{Gaussian}})
#' @param y A Density object (e.g. \code{\link{Gaussian}})
#' @return A DensityList object.
#' @export
#' @examples Gaussian() + Gaussian()
`+.Density` <- function(x, y = NULL) {
  if (!is.null(y) & !is.Density(y)) {
    stop("Error: Please use the plus sign to join two Density object")
  }

  l <-
    if (is.null(y)) {
      if (is.DensityList(x)) { x } else { list(x) }
    } else {
      if (is.DensityList(x)) { c(x, list(y)) } else { list(x, y) }
    }
  structure(l, class = c("DensityList"))
}

explain.DensityList <- function(x){
  lapply(x, explain)
}

is.DensityList <- function(x) {
  all(sapply(x, is.Density))
}

is.multivariate.DensityList <- function(x) {
  all(sapply(x, is.multivariate))
}
