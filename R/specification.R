makeNames <- function(s) {
  substr(gsub('[^a-zA-Z]', '', make.names(s)), 1, 32)
}

collapse <- function(...) {
  paste(..., sep = "\n", collapse = "\n")
}

simpleApply <- function(X, FUN, func1 = identity, ...) {
  lapply(func1(X), FUN, ...)
}

doubleApply <- function(X, FUN, func1 = identity, func2 = identity, ...) {
  simpleApply(X, function(subX) {
    collapse(
      simpleApply(subX, FUN, func1 = func1)
    )
  }, func1 = func2)
}

tripleApply <- function(X, FUN, func1 = identity, func2 = identity, func3 = identity, ...) {
  simpleApply(X, function(subX) {
    collapse(
      doubleApply(subX, FUN, func1 = func1, func2 = func2)
    )
  }, func1 = func3)
}

check         <- function(x, ...) { UseMethod("check", x) }
fill          <- function(x, ...) { UseMethod("fill", x) }
write_chunks  <- function(x, ...) { UseMethod("write_chunks", x) }
write_model   <- function(x, ...) { UseMethod("write_model", x) }

Specification <- function(l) {
  structure(l, class = "Specification")
}

explain.Specification <- function(spec) {
  sprintf("Here I'll explain my spec.")
}

check.Specification <- function(spec) {
  stop("Checks not implemented yet.")
}

write_chunks.Specification <- function(spec, writeDir = tempdir()) {
  # Uses different functions depending on the depth of the nested lists
  funObservation <- if (length(spec$observation$density) == 1) { simpleApply } else {doubleApply}
  funInitProb    <- simpleApply
  funTransition  <- if (length(spec$transition$density) == 1)  { simpleApply } else {doubleApply}
  funObsPriors   <- if (length(spec$observation$density) == 1) { doubleApply } else {tripleApply}

  # Write observation parameters
  write(
    collapse(funObservation(spec$observation$density, parameters)),
    file = file.path(writeDir, "parameters.stan")
  )

  # Write observation log-likelihood
  write(
    collapse(funObservation(spec$observation$density, loglike)),
    file = file.path(writeDir, "loglike.stan")
  )

  # Write priors (observation, transition, and initial distribution)
  write(
    collapse(
      c(
        funInitProb(spec$init_prob$density, prior),
        funTransition(spec$transition$density, prior),
        funObsPriors(spec$observation$density, prior, getParameters)
      )
    ),
    file = file.path(writeDir, "priors.stan")
  )
}

write_model <- function(spec, writeDir = tempdir()) {
  # Select best template
  baseR <- if (spec$observation$R == 1) { "univariate" } else {"multivariate"}
  baseA <- if (is.null(spec$transition$covariates)) { "homogeneous" } else {"heterogeneous"}
  base  <- file.path("src", "stan", sprintf("%s-%s.stan", baseR, baseA))

  # Create folder
  writeDir <- file.path(writeDir, makeNames(spec$name))
  if (!dir.exists(writeDir)) { dir.create(writeDir, recursive = TRUE) }

  # Write chuncks & models
  write_chunks(spec, writeDir)
  build <- rstan::stanc_builder(
    file = base,
    isystem = c(dirname(base), getwd(), writeDir)
  )

  write(
    build$model_code,
    file = file.path(writeDir, "model.stan")
  )
}
