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

#' Create a representation of a probability mass or density function for a
#' continuous univariate random variable. It can be used to specify either a
#' prior distribution for a model parameter or a likelihood function for an
#' observation model.
#'
#' @param name A character vector with the name of the density.
#' @param bounds (optional) A list with two elements specifying the lower and upper bound for the parameter space. Use either a fixed value for a finite bound or NULL for no bounds. It defaults to an unbounded parameter space.
#' @param trunc (optional) A list with two elements specifying the lower and upper bound for the domain of the density function. Use either a fixed value for a finite bound or NULL for no truncation. It defaults to an unbounded domain.
#' @param k (optional) The number of the hidden state for which this density should be used. This argument is mostly for internal use: you should not use it unless you are acquainted with the internals of this software.
#' @param r (optional) The dimension of the observation vector dimension for which this density should be used. This argument is mostly for internal use: you should not use it unless you are acquainted with the internals of this software.
#' @param param (optional) The name of the parameter. This argument is mostly for internal use: you should not use it unless you are acquainted with the internals of this software.
#' @param ...  Other arguments for the density.
#'
#' #'
#' @return A Density object.
#' @family Density
#' @note The examples are merely illustrative and should not be taken for prior choice recommendations. If you are looking for some, you may start with \href{ https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations}{Stan's Prior Choice Recommendation}.
Density <- function(name, bounds = list(NULL, NULL), trunc  = list(NULL, NULL),
                    k = NULL, r = NULL, param = NULL, ...) {
  # Evaluate nested expressions (Densities)
  dots <- list(...)
  for (i in seq_along(dots)) {
    if (is.language(dots[[i]])) {
      dots[[i]] <- eval(dots[[i]])
    }
  }

  densityParams <- c(dots, list(bounds = bounds, trunc = trunc, k = k, r = r, param = param))

  # # Check for generic parameters
  # check_list(dots[["bounds"]], 2, "bounds")
  # check_list(dots[["trunc"]], 2, "trunc")

  structure(
    c(list(name = name), densityParams),
    class = c(name, "Density")
  )
}

#' Explain a Density object in human readable format.
#'
#' @param x A Density object.
#' @param print An optional logical indicating whether the description should be printing out.
#' @return A character vector.
explain_density            <- function(x, print = FALSE) { UseMethod("explain_density", x) }

#' #'
explain_density.Density <- function(x, print = FALSE) {
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
          paramName, explain_density(freeParam[[paramName]])
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

  strOut <- collapse(c(block1, block2, block3))

  if (print)
    cat(strOut)

  invisible(strOut)
}

#' Return the names of both free and fixed parameters in the Density object.
#'
#' @param x A Density object.
#' @return A vector of character vectors.
#' @examples
#' \dontrun{
#' getParameterNames(
#'   Gaussian(
#'     mu    = Gaussian(mu = 0, sigma = 10),
#'     sigma = 1
#'   )
#' )
#' }
getParameterNames  <- function(x) { UseMethod("getParameterNames", x) }

#' #'
getParameterNames.Density <- function(x) {
  warning(
    sprintf(
      "getParameterNames not implemented for the %s density.",
      x$name
    )
  )
}

#' Return the names of the free parameters in the Density object.
#'
#' @param x A Density object.
#' @return A vector of character vectors.
#' @examples
#' \dontrun{
#' getFreeParameters(
#'   Gaussian(
#'     mu    = Gaussian(mu = 0, sigma = 10),
#'     sigma = 1
#'   )
#' )
#' }
getFreeParameters  <- function(x) { UseMethod("getFreeParameters", x) }

#' #'
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

#' Return the names of the fixed parameters in the Density object.
#'
#' @param x A Density object.
#' @return A vector of character vectors.
#' @examples
#' \dontrun{
#' getFixedParameters(
#'   Gaussian(
#'     mu    = Gaussian(mu = 0, sigma = 10),
#'     sigma = 1
#'   )
#' )
#' }
getFixedParameters <- function(x) { UseMethod("getFixedParameters", x) }

#' #'
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

#' Check if it is a Density object for the transition model.
#'
#' @param x a Density object.
#' @return TRUE if the object is meant for transition models, FALSE otherwise.
is.link            <- function(x) { UseMethod("is.link", x) }

#' #'
is.link.Density                     <- function(x) { FALSE }

#' Check if it is a Density object for discrete random variables.
#'
#' @param x a Density object.
#' @return TRUE if the object is meant to represent a discrete random variables, FALSE otherwise.
is.discrete        <- function(x) { UseMethod("is.discrete", x) }

#' #'
is.discrete.Density                 <- function(x) { FALSE }

#' Check if it is a Density object for multivariate random variables.
#'
#' @param x a Density object.
#' @return TRUE if the object is meant to represent a multivariate random variables, FALSE otherwise.
is.multivariate    <- function(x) { UseMethod("is.multivariate", x) }

#' #'
is.multivariate.Density             <- function(x) { FALSE }

#' Check if it is a Density object.
#'
#' @param x An object.
#' @return TRUE if the object is a Density objectm FALSE otherwise.
is.Density <- function(x) {
  "Density" %in% class(x)
}

#' #'
block_data.Density <- function(x, noLogLike) {
  if (noLogLike) {
    "// No observation vector"
  } else {
    "matrix[T, R] y;  // observations"
  }
}

#' #'
noLogLike.Density <- function(x) {
  sprintf("loglike[%s][t] = 1;", x$k)
}

#' #'
is.TVInitial.Density                <- function(x) { FALSE }

#' #'
is.TVTransition.Density             <- function(x) { FALSE }

#' #'
constants.Density                   <- function(x) { ""    }

#' #'
link.Density                        <- function(x) { ""    }

#' Create a representation of a probability mass or density function for a
#' continuous multivariate random variable. It can be used to specify either a
#' prior distribution for a model parameter or a likelihood function for an
#' observation model.
#'
#' @inherit Density
#' @family MultivariateDensity
MultivariateDensity <- function(name, ...) {
  x <- Density(name, ...)
  class(x) <- append(class(x), "MultivariateDensity", 1)
  x
}

#' Create a representation of a probability mass or density function for a
#' discrete univariate random variable. It can be used to specify either a
#' prior distribution for a model parameter or a likelihood function for an
#' observation model.
#'
#' @inherit Density
#' @family DiscreteDensity
DiscreteDensity <- function(name, ...) {
  x <- Density(name, ...)
  class(x) <- append(class(x), "DiscreteDensity", 1)
  x
}

#' #'
is.discrete.DiscreteDensity         <- function(x) { TRUE  }

#' #'
block_data.DiscreteDensity <- function(x, noLogLike) {
  if (noLogLike) {
    "// No observation vector"
  } else {
    "int y[T, R];     // observations"
  }
}

#' Create a representation of a probability mass or density function for a
#' discrete multivariate random variable. It can be used to specify either a
#' prior distribution for a model parameter or a likelihood function for an
#' observation model.
#'
#' @inherit Density
#' @family MultivariateDiscreteDensity
MultivariateDiscreteDensity <- function(name, ...) {
  x <- DiscreteDensity(name, ...)
  class(x) <- append(class(x), "MultivariateDensity", 1)
  x
}

#' #'
is.multivariate.MultivariateDensity <- function(x) { TRUE  }

#' Create a representation of a probability mass or density function for a
#' univariate random variable that may be used only to specify a prior
#' distribution for a model parameter.
#'
#' @inherit Density
#' @family PriorOnlyDensity
PriorOnlyDensity <- function(name, ...) {
  x <- Density(name, ...)
  class(x) <- append(class(x), "PriorOnlyDensity", 1)
  x
}

#' Create a representation of a probability mass or density function for a
#' multivariate random variable that may be used only to specify a prior
#' distribution for a model parameter.
#'
#' @inherit Density
#' @family PriorOnlyDensity
PriorOnlyMultivariateDensity <- function(name, ...) {
  x <- MultivariateDensity(name, ...)
  class(x) <- append(class(x), "PriorOnlyDensity", 1)
  x
}

#' #'
freeParameters.PriorOnlyDensity     <- function(x) { ""    }

#' #'
fixedParameters.PriorOnlyDensity    <- function(x) { ""    }

#' #'
generated.PriorOnlyDensity          <- function(x) { ""    }

#' #'
getFreeParameters.PriorOnlyDensity  <- function(x) { ""    }

#' #'
logLike.PriorOnlyDensity            <- function(x) { ""    }

#' Create a representation of a link function that may be used only to specify
#' a transition model.
#'
#' @inherit Density
#' @family LinkDensity
LinkDensity <- function(name, ...) {
  x <- Density(name, ...)
  class(x) <- append(class(x), "LinkDensity", 1)
  x
}

#' #'
is.link.LinkDensity                 <- function(x) { TRUE  }

# DensityList -------------------------------------------------------------
#' Append two Density objects.
#'
#' @aliases +
#' @usage x + y
#' @param x A Density object (e.g. \code{\link{Gaussian}})
#' @param y A Density object (e.g. \code{\link{Gaussian}})
#' @return A DensityList object.
#' #'
#' @examples Gaussian(0, 1) + Gaussian(0, 1)
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

#' #'
explain_density.DensityList <- function(x, print = FALSE){
  lapply(x, explain_density, print = print)
}

is.DensityList <- function(x) {
  all(sapply(x, is.Density))
}

is.discrete.DensityList <- function(x) {
  all(sapply(x, is.discrete))
}

is.multivariate.DensityList <- function(x) {
  all(sapply(x, is.multivariate))
}
