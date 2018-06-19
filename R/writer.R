write_stanfile <- function(code, dir, filename, ...) {
  write(
    collapse(code, ...),
    file = file.path(dir, filename)
  )
}

write_functions <- function(spec, writeDir) {
  write_stanfile(
    block_functions(spec),
    writeDir,
    "functions.stan"
  )
}

write_data <- function(spec, noLogLike, writeDir) {
  strK    <- "int<lower = 1> K; // number of hidden states"
  strR    <- "int<lower = 1> R; // dimension of the observation vector"
  strSpec <- block_data(spec)
  strObs  <- densityCollect(spec$observation$density, data, noLogLike = noLogLike)
    # if (noLogLike) {
    #   "// No observation vector"
    # } else {
    #   densityCollect(spec$observation$density, data, noLogLike = noLogLike)
    # }

  write_stanfile(
    c(strK, strR, strSpec, strObs),
    writeDir,
    "data.stan"
  )
}

write_tdata <- function(spec, writeDir) {
  write_stanfile(
    block_tdata(spec),
    writeDir,
    "tdata.stan"
  )
}

write_constants <- function(spec, writeDir) {
  write_stanfile(
    densityCollect(spec$observation$density, constants),
    writeDir,
    "constants.stan"
  )
}

write_parameters <- function(spec, writeDir) {
  write_stanfile(
    c(
      densityApply(spec$observation$density, fixedParameters),
      densityApply(spec$initial$density, fixedParameters),
      densityApply(spec$transition$density, fixedParameters)
    ),
    writeDir,
    "fixedParameters.stan"
  )

  write_stanfile(
    c(
      densityApply(spec$observation$density, freeParameters),
      densityApply(spec$initial$density, freeParameters),
      densityApply(spec$transition$density, freeParameters)
    ),
    writeDir,
    "freeParameters.stan"
  )

  write_stanfile(
    block_parameters(spec),
    writeDir,
    "parameters.stan"
  )
}

write_tparameters <- function(spec, writeDir) {
  write_stanfile(
    block_tparameters(spec),
    writeDir,
    "tparameters.stan"
  )
}

write_logLikelihood <- function(spec, noLogLike, writeDir) {
  funLogLike <- match.fun(if (noLogLike) "noLogLike" else "logLike")
  write_stanfile(
    densityApply(spec$observation$density, funLogLike),
    writeDir,
    "logLikelihood.stan"
  )
}

write_link <- function(spec, writeDir) {
  write_stanfile(
    if (all(densityApply(spec$initial$density, is.link))) {
      densityApply(spec$initial$density, link)
      } else {
        ""
      },
    writeDir,
    "initialLink.stan"
  )

  write_stanfile(
    if (all(densityApply(spec$transition$density, is.link))) {
      densityApply(spec$transition$density, link)
    } else {
      ""
    },
    writeDir,
    "transitionLink.stan"
  )
}

write_target <- function(spec, writeDir) {
  write_stanfile(
    chunk_calculate_target(spec),
    writeDir,
    "calculate-target.stan"
  )

  write_stanfile(
    chunk_increase_target(spec),
    writeDir,
    "increase-target.stan"
  )
}

write_priors <- function(spec, writeDir) {
  initPriors <-
    if (all(!densityApply(spec$initial$density, is.link))) {
      densityApply(spec$initial$density, prior)
    } else {
      ""
    }

  tranPriors <-
    if (all(!densityApply(spec$transition$density, is.link))) {
      densityApply(spec$transition$density, prior)
    } else {
      ""
    }
    # densityApply(spec$transition$density, prior)
  obsPriors  <- densityApply(
    spec$observation$density,
    function(x) {
      paramList <- getFreeParameters(x)
      if (is.empty(paramList)) {
        ""
      } else {
        densityApply(paramList, prior)
      }
    }
  )

  write_stanfile(
    c(initPriors, tranPriors, obsPriors),
    writeDir,
    "priors.stan"
  )
}

write_generated <- function(spec, writeDir) {
  write_stanfile(
    block_generated(spec),
    writeDir,
    "generated.stan"
  )
}

write_zpredictive <- function(spec, writeDir) {
  write_stanfile(
    chunk_zpredictive(spec),
    writeDir,
    "zpredictive.stan"
  )
}

write_ypredictive <- function(spec, writeDir) {
  write_stanfile(
    densityApply(spec$observation$density, generated),
    writeDir,
    "ypredictive.stan"
  )
}

write_chunks.Specification <- function(spec, noLogLike, writeDir) {
  write_functions(spec, writeDir)
  write_data(spec, noLogLike, writeDir)
  write_tdata(spec, writeDir)
  write_constants(spec, writeDir)
  write_link(spec, writeDir)
  write_parameters(spec, writeDir)
  write_tparameters(spec, writeDir)
  write_logLikelihood(spec, noLogLike, writeDir)
  write_target(spec, writeDir)
  write_priors(spec, writeDir)
  write_generated(spec, writeDir)
  write_zpredictive(spec, writeDir)
  write_ypredictive(spec, writeDir)
}

write_model.Specification <- function(spec, noLogLike, writeDir) {
  # Select best template
  base  <- system.file(
    file.path("stan", "template.stan"),
    package = "BayesHMM"
  )

  # Create folder
  writeDir <- file.path(writeDir, make_names(spec$name))
  if (!dir.exists(writeDir)) { dir.create(writeDir, recursive = TRUE) }

  # Write chuncks
  write_chunks(spec, noLogLike, writeDir)

  # Unify all chunks into one single Stan model
  build <- rstan::stanc_builder(
    file = base,
    isystem = c(dirname(base), file.path(dirname(base), "chunks"), writeDir)
  )

  # Write model
  write(
    build$model_code,
    file = file.path(writeDir, "model.stan")
  )

  return(file.path(writeDir, "model.stan"))
}
