write_stanfile <- function(code, dir, filename, ...) {
  write(
    collapse(code, ...),
    file = file.path(dir, filename)
  )
}

write_data <- function(spec, noLogLike, writeDir) {
  strData <-
    if (noLogLike) {
      "// No observation vector"
    } else {
      if (is.discrete(spec)) {
        sprintf("int y[T, %s]; // observations", spec$observation$R)
      } else {
        sprintf("matrix[T, %s] y; // observations", spec$observation$R)
      }
    }

  write_stanfile(
    strData,
    writeDir,
    "data.stan"
  )
}

write_constants <- function(spec, writeDir) {
  write_stanfile(
    c(
      sprintf("int K = %s; // number of hidden states", spec$K),
      sprintf("int R = %s; // dimension of the observation vector", spec$observation$R),
      unique(densityApply(spec$observation$density, constants))
    ),
    writeDir,
    "constants.stan"
  )
}

write_parameters <- function(spec, writeDir) {
  write_stanfile(
    densityApply(spec$observation$density, freeParameters),
    writeDir,
    "freeParameters.stan"
  )

  write_stanfile(
    densityApply(spec$observation$density, fixedParameters),
    writeDir,
    "fixedParameters.stan"
  )
}

write_priors <- function(spec, writeDir) {
  initPriors <- densityApply(spec$init_prob$density, prior)
  tranPriors <- densityApply(spec$transition$density, prior)
  obsPriors  <- densityApply(
    spec$observation$density,
    function(x) {
      densityApply(getFreeParameters(x), prior)
    }
  )

  write_stanfile(
    c(initPriors, tranPriors, obsPriors),
    writeDir,
    "priors.stan"
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

write_ypredictive <- function(spec, writeDir) {
  write_stanfile(
    densityApply(spec$observation$density, generated),
    writeDir,
    "ypredictive.stan"
  )
}

write_chunks.Specification <- function(spec, noLogLike, writeDir) {
  write_data(spec, noLogLike, writeDir)
  write_constants(spec, writeDir)
  write_parameters(spec, writeDir)
  write_priors(spec, writeDir)
  write_logLikelihood(spec, noLogLike, writeDir)
  write_ypredictive(spec, writeDir)
}

write_model.Specification <- function(spec, noLogLike, writeDir) {
  # Select best template
  baseR <- if (is.multivariate(spec)) { "univariate" } else {"univariate"}
  baseA <- if (is.null(spec$transition$covariates)) { "Homogeneous" } else {"Heterogeneous"}
  base  <- system.file(
    file.path("stan", sprintf("%s%s.stan", baseR, baseA)),
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
    isystem = c(dirname(base), writeDir)
  )

  # Write model
  write(
    build$model_code,
    file = file.path(writeDir, "model.stan")
  )

  return(file.path(writeDir, "model.stan"))
}
