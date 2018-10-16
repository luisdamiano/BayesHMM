# Stan code blocks
block_functions   <- function(x) { UseMethod("block_functions", x) }
block_data        <- function(x, ...) { UseMethod("block_data", x) }
block_tdata       <- function(x) { UseMethod("block_tdata", x) }
block_parameters  <- function(x) { UseMethod("block_parameters", x) }
block_tparameters <- function(x) { UseMethod("block_tparameters", x) }
block_generated   <- function(x) { UseMethod("block_generated", x) }
block_target      <- function(x) { UseMethod("block_target", x) }

# Stan included chunks
chunk_calculate_target <- function(x) { UseMethod("chunk_calculate_target", x) }
chunk_increase_target  <- function(x) { UseMethod("chunk_increase_target", x) }
chunk_zpredictive      <- function(x) { UseMethod("chunk_zpredictive", x) }

# Other internals
write_chunks      <- function(spec, ...) { UseMethod("write_chunks", spec) }
write_model       <- function(spec, ...) { UseMethod("write_model", spec) }
make_data         <- function(spec, ...) { UseMethod("make_data", spec) }
is.TVTransition   <- function(x) { UseMethod("is.TVTransition", x) }
is.TVInitial      <- function(x) { UseMethod("is.TVInitial", x) }

# Default methods for an empty Specification
block_functions.Specification         <- function(x) { "" }
block_data.Specification              <- function(x) { "" }
block_tdata.Specification             <- function(x) { "" }
block_parameters.Specification        <- function(x) { "" }
block_tparameters.Specification       <- function(x) { "" }
block_generated.Specification         <- function(x) { "" }
block_target.Specification            <- function(x) { "" }
chunk_calculate_target.Specification  <- function(x) { "" }
chunk_increase_target.Specification   <- function(x) { "" }
chunk_zpredictive.Specification       <- function(x) { "" }

is.multivariate.Specification <- function(spec) {
  all(densityApply(spec$observation$density, is.multivariate))
}

is.discrete.Specification <- function(spec) {
  all(densityApply(spec$observation$density, is.discrete))
}

is.TVTransition.Specification <- function(spec) {
  all(densityApply(spec$transition$density, is.link))
}

is.TVInitial.Specification <- function(spec) {
  all(densityApply(spec$initial$density, is.link))
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
