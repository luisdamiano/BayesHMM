check         <- function(x, ...) { UseMethod("check", x) }
run           <- function(x, ...) { UseMethod("run", x) }
write_chunks  <- function(x, ...) { UseMethod("write_chunks", x) }
write_model   <- function(x, ...) { UseMethod("write_model", x) }
make_data     <- function(spec, ...) { UseMethod("make_data", spec) }

block_functions   <- function(x) { UseMethod("block_functions", x) }
block_data        <- function(x) { UseMethod("block_data", x) }
block_tdata       <- function(x) { UseMethod("block_tdata", x) }
block_parameters  <- function(x) { UseMethod("block_parameters", x) }
block_tparameters <- function(x) { UseMethod("block_tparameters", x) }
block_generated   <- function(x) { UseMethod("block_generated", x) }
block_target      <- function(x) { UseMethod("block_target", x) }

chunk_calculate_target <- function(x) { UseMethod("chunk_calculate_target", x) }
chunk_increase_target  <- function(x) { UseMethod("chunk_increase_target", x) }
chunk_zpredictive      <- function(x) { UseMethod("chunk_zpredictive", x) }

is.TVTransition <- function(x) { UseMethod("is.TVTransition", x) }
is.TVInitial <- function(x) { UseMethod("is.TVInitial", x) }

spec <- function(K, R, observation = NULL, initial = NULL,
                 transition = NULL, name = "") {
  l <- list(
    name = name,
    K    = K,
    observation = list(
      R = R,
      covariates = NULL,
      density = parse_observation(observation, K, R)
    ),
    initial   = list(
      density = parse_initial(initial, K, R)
    ),
    transition  = list(
      covariates = NULL,
      density = parse_transition(transition, K, R)
    )
  )

  spec <- structure(l, class = "Specification")

  check(spec)

  spec
}

block_functions.Specification   <- function(x) { "" }
block_data.Specification        <- function(x) { "" }
block_tdata.Specification       <- function(x) { "" }
block_parameters.Specification  <- function(x) { "" }
block_tparameters.Specification <- function(x) { "" }
block_generated.Specification   <- function(x) { "" }
block_target.Specification      <- function(x) { "" }
chunk_calculate_target.Specification <- function(x) { "" }
chunk_increase_target.Specification  <- function(x) { "" }
chunk_zpredictive.Specification <- function(x) { "" }

check.Specification <- function(spec) {
  # Check if R and the observation tree are consistent
  if (spec$observation$R == 1 & is.multivariate(spec)) {
    stop("Inconsistent specification: although R was set to 1, a multivariate density was given.")
  }

  # if (spec$observation$R != 1 & !is.multivariate(spec)) {
  #   stop(
  #     sprintf(
  #       "Inconsistent specification: although R is set to %s, a univariate density was given.",
  #       spec$observation$R
  #     )
  #   )
  # }

  # Check if univariate and mulvariate densities are mixed
  dens <- densityApply(spec$observation$density, is.multivariate)
  if (length(unique(dens)) != 1) {
    stop("Inconsistent specification: univariate and multivariate densities for the observation model cannot be mixed.")
  }

  # Check if fixed parameters are well specified
  invisible(
    densityApply(spec$observation$density, fixedParameters)
  )
}

explain.Specification <- function(spec) {
  stop("TO BE IMPLEMENTED.")
}

run.Specification <- function(spec, data = NULL, control = NULL,
                              writeDir = tempdir(), ...) {
  stanData <- data
  stanFile <- write_model(spec, noLogLike = is.null(data$y), writeDir)

  stanDots <- c(
    list(...),
    list(
      file       = stanFile,
      data       = stanData,
      model_name = spec$name
    ),
    control
  )

  fit <- do.call(rstan::stan, stanDots)
  attr(fit, "BayesHMM.filename") <- stanFile

  return(fit)
}

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
  all(densityApply(spec$transition$density, is.link))
}

make_data.Specification <- function(spec, y = NULL, xBeta = NULL, T = NULL) {
  stanData <- list(
    K = spec$K,
    R = spec$observation$R
  )

  # Each specification should work its own elements, then call nextMethod()

  if (!is.null(y)) {
    stanData[["T"]] <- NROW(y)
    stanData[["y"]] <- y
  } else {
    if (is.null(T)) {
      stanData[["T"]] <- 1E3
    } else {
      stanData[["T"]] <- T
    }
  }

  M <- unique(densityApply(spec$observation$density, "[[", "M"))
  if (is.numeric(M) & !is.null(M)) {
    stanData[["M"]] <- M
  }

  if (!is.null(xBeta)) {
    stanData[["x"]] <- xBeta
  }

  stanData
}

setGeneric(
  "stan_file",
  function(object) {
    attr(object, "BayesHMM.filename")
  }
)

methods::setGeneric("stan_file")

methods::setMethod("stan_file", signature(object = "stanfit"), function(object) {
  attr(object, "BayesHMM.filename")
})
