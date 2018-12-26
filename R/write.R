# Internal functions to write a specification into Stan code --------------
# This file contains internal functions that write a specification into
# Stan code.
# We regret to say that these functions are currently undocumented :(.

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
  strSpec <- block_data(spec)
  strObs  <- densityCollect(
    spec$observation$density, block_data, noLogLike = noLogLike
  )

  strTrans <- ""
  # strTrans <- densityCollect(
  #   spec$transition$density, block_data, noLogLike = noLogLike
  # )

  write_stanfile(
    unique(c(strSpec, strObs, strTrans)),
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
      densityCollect(spec$transition$density, fixedParameters)#,
      # densityApply(spec$initial$density, fixedParameters),
      # densityApply(spec$transition$density, fixedParameters)
      # pseudo-code to have fixed parameters in the transition:
      # hmm(..., transition = matrix(c(0.1, 0.2, 0.3, ...)))
      # pseudo-code to have fixed parameters in the initial:
      # hmm(..., initial = c(0.1, 0.2, 0.7))
    ),
    writeDir,
    "fixedParameters.stan"
  )

  write_stanfile(
    c(
      densityApply(spec$observation$density, freeParameters),
      if (is.TVInitial(spec)) {
        densityCollect(spec$initial$density, freeParameters)
      } else {
        ""
      },
      if (is.TVTransition(spec)) {
        densityCollect(spec$transition$density, freeParameters)
      } else {
        ""
      }
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
      densityCollect(spec$initial$density, link)
      } else {
        ""
      },
    writeDir,
    "initialLink.stan"
  )

  write_stanfile(
    if (all(densityApply(spec$transition$density, is.link))) {
      densityCollect(spec$transition$density, link)
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
  getFreeParameterPriors <- function(x) {
    paramList <- getFreeParameters(x)
    if (is.empty(paramList)) {
      ""
    } else {
      densityApply(paramList, prior)
    }
  }

  initPriors <- densityApply(spec$init_prob$density, prior)
  tranPriors <- densityApply(spec$transition$density, prior)

  obsPriors  <- densityApply(
    spec$observation$density,
    getFreeParameterPriors
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

#' @inherit write_chunks
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

#' @inherit write_model
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
