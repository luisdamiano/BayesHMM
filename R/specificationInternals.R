# Internal functions to write a specification into Stan code --------------
# This file contains internal functions that write a specification into
# Stan code.
# We regret to say that these functions are currently undocumented :(.

# Stan code blocks

#' Write the function block of the Stan code.
#'
#' @param x A \code{\link{Density}} or a \code{\link{Specification}} object.
#' @return A character vector with the Stan code.
block_functions   <- function(x) { UseMethod("block_functions", x) }

#' Write the data block of the Stan code.
#'
#' @param x A \code{\link{Density}} or a \code{\link{Specification}} object.
#' @param noLogLike A logical stating whether the log-likelihood should be excluded from the program. If TRUE, the Stan code will draw samples from the prior predictive density. If FALSE, the Stan code will draw samples from the posterior predictive density.
#' @return A character vector with the Stan code.
block_data        <- function(x, noLogLike) { UseMethod("block_data", x) }

#' Write the transformed data block of the Stan code.
#'
#' @param x A \code{\link{Density}} or a \code{\link{Specification}} object.
#' @return A character vector with the Stan code.
block_tdata       <- function(x) { UseMethod("block_tdata", x) }

#' Write the parameters block of the Stan code.
#'
#' @param x A \code{\link{Density}} or a \code{\link{Specification}} object.
#' @return A character vector with the Stan code.
block_parameters  <- function(x) { UseMethod("block_parameters", x) }

#' Write the transformed parameters block of the Stan code.
#'
#' @param x A \code{\link{Density}} or a \code{\link{Specification}} object.
#' @return A character vector with the Stan code.
block_tparameters <- function(x) { UseMethod("block_tparameters", x) }

#' Write the generated quantities block of the Stan code.
#'
#' @param x A \code{\link{Density}} or a \code{\link{Specification}} object.
#' @return A character vector with the Stan code.
block_generated   <- function(x) { UseMethod("block_generated", x) }

#' Write the model block of the Stan code.
#'
#' @param x A \code{\link{Density}} or a \code{\link{Specification}} object.
#' @return A character vector with the Stan code.
block_target      <- function(x) { UseMethod("block_target", x) }

# Stan included chunks
chunk_calculate_target <- function(x) { UseMethod("chunk_calculate_target", x) }
chunk_increase_target  <- function(x) { UseMethod("chunk_increase_target", x) }
chunk_zpredictive      <- function(x) { UseMethod("chunk_zpredictive", x) }

# Other internals

#' Write Stan code chunks to disk
#'
#' @param spec An object returned by either \code{\link{specify}} or \code{\link{hmm}}.
#' @param noLogLike A logical stating whether the log-likelihood should be excluded from the program. If TRUE, the Stan code will draw samples from the prior predictive density. If FALSE, the Stan code will draw samples from the posterior predictive density.
#' @param writeDir A character vector with the path where the Stan file should be written. Useful to inspect and modify the Stan code manually.
#' @return Nothing.
write_chunks      <- function(spec, noLogLike, writeDir) {
  UseMethod("write_chunks", spec)
}

#' Write a ready-to-compile Stan file to disk
#'
#' @param spec An object returned by either \code{\link{specify}} or \code{\link{hmm}}.
#' @param noLogLike A logical stating whether the log-likelihood should be excluded from the program. If TRUE, the Stan code will draw samples from the prior predictive density. If FALSE, the Stan code will draw samples from the posterior predictive density.
#' @param writeDir A character vector with the path where the Stan file should be written. Useful to inspect and modify the Stan code manually.
#' @return A character vector with the path to the filename.
write_model       <- function(spec, noLogLike, writeDir) {
  UseMethod("write_model", spec)
}

make_data         <- function(spec, ...) { UseMethod("make_data", spec) }

#' Check if it is a time-varying transition probability object.
#'
#' @param x A \code{\link{Density}} or a \code{\link{Specification}} object.
#' @return TRUE if the object is a time-varying transition probability object, FALSE otherwise.
is.TVTransition   <- function(x) { UseMethod("is.TVTransition", x) }

#' Check if it is a time-varying initial distribution object.
#'
#' @param x A \code{\link{Density}} or a \code{\link{Specification}} object.
#' @return TRUE if the object is a time-varying initial distribution object, FALSE otherwise.
is.TVInitial      <- function(x) { UseMethod("is.TVInitial", x) }

# Default methods for an empty Specification
#' @inherit block_functions
block_functions.Specification         <- function(x) { "" }

#' @inherit block_data
block_data.Specification              <- function(x) { "" }

#' @inherit block_tdata
block_tdata.Specification             <- function(x) { "" }

#' @inherit block_parameters
block_parameters.Specification        <- function(x) { "" }

#' @inherit block_tparameters
block_tparameters.Specification       <- function(x) { "" }

#' @inherit block_generated
block_generated.Specification         <- function(x) { "" }

#' @inherit block_target
block_target.Specification            <- function(x) { "" }

chunk_calculate_target.Specification  <- function(x) { "" }
chunk_increase_target.Specification   <- function(x) { "" }
chunk_zpredictive.Specification       <- function(x) { "" }

#' @inherit is.discrete
is.discrete.Specification <- function(x) {
  all(densityApply(x$observation$density, is.discrete))
}

#' @inherit is.multivariate
is.multivariate.Specification <- function(x) {
  all(densityApply(x$observation$density, is.multivariate))
}

#' @inherit is.TVTransition
is.TVTransition.Specification <- function(x) {
  all(densityApply(x$transition$density, is.link))
}

#' @inherit is.TVInitial
is.TVInitial.Specification <- function(x) {
  all(densityApply(x$initial$density, is.link))
}

make_data.Specification <- function(spec, y = NULL, x = NULL, u = NULL,
                                    v = NULL, T = NULL) {
  # x = covariates for observation model  M dimension
  # u = covariates for transition model   P dimension
  # v = covariates for initial model      Q dimension
  stanData <- list(
    K = spec$K,
    R = spec$observation$R
  )

  if (is.null(y)) {
    if (is.null(T)) {
      stanData[["T"]] <- 1E3
    } else {
      stanData[["T"]] <- T
    }
  } else {
    stanData[["T"]] <- NROW(y)
    stanData[["y"]] <- cast_to_matrix(y, nRow = stanData[["T"]], nCol = stanData[["R"]])
  }

  # Covariates in the observation model
  M <- unique(densityApply(spec$observation$density, "[[", "M"))[[1]]
  if (!is.null(M) && check_natural(M)) {
    stanData[["M"]] <- M

    if (!is.null(x))
      stanData[["x"]] <- cast_to_matrix(x, stanData[["T"]], M)
  }

  # Covariates in the transition model
  P <- unique(densityApply(spec$transition$density, "[[", "P"))[[1]]
  if (!is.null(P) && check_natural(P)) {
    stanData[["P"]] <- P

    if (!is.null(u))
      stanData[["u"]] <- cast_to_matrix(u, stanData[["T"]], P)
  }

  # Covariates in the initial distribution model
  Q <- unique(densityApply(spec$initial$density, "[[", "Q"))[[1]]
  if (!is.null(Q) && check_natural(Q)) {
    stanData[["Q"]] <- Q

    if (!is.null(v))
      stanData[["v"]] <- cast_to_matrix(v, stanData[["T"]], Q)
  }

  stanData
}
