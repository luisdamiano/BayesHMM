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

# Other methods for Specification objects
check             <- function(spec, ...) { UseMethod("check", spec) }
explain           <- function(spec, ...) { UseMethod("explain", spec) }
explain_observation <- function(spec, ...) { UseMethod("explain_observation", spec) }
explain_initial     <- function(spec, ...) { UseMethod("explain_initial", spec) }
explain_transition  <- function(spec, ...) { UseMethod("explain_transition", spec) }
run               <- function(spec, ...) { UseMethod("run", spec) }
fit               <- function(spec, ...) { UseMethod("fit", spec) }
sim               <- function(spec, ...) { UseMethod("sim", spec) }
compile           <- function(spec, ...) { UseMethod("compile", spec) }
sampling          <- function(spec, ...) { UseMethod("sampling", spec) }
optimizing        <- function(spec, ...) { UseMethod("optimizing", spec) }
write_chunks      <- function(spec, ...) { UseMethod("write_chunks", spec) }
write_model       <- function(spec, ...) { UseMethod("write_model", spec) }
make_data         <- function(spec, ...) { UseMethod("make_data", spec) }
browse_model      <- function(x) { UseMethod("browse_model", x) }
is.TVTransition   <- function(x) { UseMethod("is.TVTransition", x) }
is.TVInitial      <- function(x) { UseMethod("is.TVInitial", x) }

spec <- function(K, R, observation = NULL, initial = NULL,
                 transition = NULL, name = "") {
  check_natural(K, "K")
  check_natural(R, "R")

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

  # check(spec)

  spec
}

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

check.Specification <- function(spec) {
  stop("TO BE IMPLEMENTED.")

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

explain.Specification <- function(spec, observation = TRUE, initial = TRUE,
                                  transition = TRUE, print = TRUE) {
  strHeader      <- make_text_header(spec$name)
  strObservation <- if (observation) { explain_observation(spec) }
  strInitial     <- if (initial)     { explain_initial(spec) }
  strTransition  <- if (transition)  { explain_transition(spec) }
  strFooter      <- sprintf(
    "Note for reproducibility: \n%s.\n",
    get_package_info()
  )

  out <- gsub(
    "\\t",
    get_print_settings()$tab,
    collapse(strHeader, strObservation, strInitial, strTransition, strFooter)
  )

  if (print) { cat(out) }

  invisible(out)
}

compile.Specification <- function(spec, priorPredictive = FALSE,
                                  writeDir = tempdir(), ...) {

  stanFile <- write_model(spec, noLogLike = priorPredictive, writeDir)

  stanDots <- c(
    list(...),
    list(
      file       = stanFile,
      model_name = spec$name
    )
  )

  stanModel <- do.call(rstan::stan_model, stanDots)
  attr(stanModel, "filename") <- stanFile
  attr(stanModel, "spec") <- spec

  return(stanModel)
}

sampling.Specification <- function(spec, stanModel = NULL, y, x = NULL, u = NULL, v = NULL,
                                   control = NULL, writeDir = tempdir(), ...) {

  if (is.null(stanModel)) {
    stanModel <- compile(spec, priorPredictive = FALSE, writeDir, ...)
  }

  stanData <- make_data(spec, y, x, u, v)
  stanDots <- c(list(...), list(object = stanModel, data = stanData))

  stanSampling <- do.call(rstan::sampling, stanDots)
  attr(stanSampling, "data")     <- stanData
  attr(stanSampling, "filename") <- attr(stanModel, "filename")
  attr(stanSampling, "spec")     <- spec

  return(stanSampling)
}

optimizing_run  <- function(stanDots, n) {
  # sink(tempfile())

  stanDots[["seed"]] <-
    if ("seed" %in% names(stanDots)) {
    stanDots[["seed"]] + n
  } else {
    sample.int(.Machine$integer.max, 1)
  }

  sysTime <- system.time({
    stanoptim <- do.call(rstan::optimizing, stanDots)
  })
  attr(stanoptim, "systemTime") <- sysTime
  attr(stanoptim, "seed")       <- stanDots[["seed"]]
  structure(stanoptim, class = c("Optimization", "list"))

  # sink()
}

optimizing_all  <- function(stanDots, nRuns, nCores) {
  l <- if (nCores == 1) {
    lapply(seq_len(nRuns), function(n) optimizing_run(stanDots, n))
  } else {
    cl <- parallel::makeCluster(nCores, outfile = "")
    doParallel::registerDoParallel(cl)
    on.exit({parallel::stopCluster(cl)})
    foreach::foreach(n = seq_len(nRuns), .combine = c, .packages = c("rstan")) %dopar% {
      optimizing_run(stanDots, n)
    }
  }
  l <- lapply(seq_len(nRuns), function(n) optimizing_run(stanDots, n))
  structure(l, class = c("OptimizationList", "list"))
}

optimizing_best <- function(stanDots, nRuns, nCores) {
  best <- optimizing_run(stanDots, n = 1)

  for (n in seq_len(nRuns)[-1]) {
    current <- optimizing_run(stanDots, n)
    if (current$return_code == 0 && current$value > best$value)
      best <- current
  }

  if (best$return_code) # from ?optimizing: Anything != 0 is problematic.
    stop(
      sprintf(
        "After %d runs, none returned a code == 0. First iteration returned %d",
        nRuns, best$return_code
      )
    )

  best
}

optimizing.Specification <- function(spec, stanModel = NULL, y, x = NULL, u = NULL, v = NULL,
                                     nRuns = 1, keep = "best", nCores = 1,
                                     control = NULL, writeDir = tempdir(), ...) {

  if (!(keep %in% c("best", "all")))
    stop("keep must be either \"best\" or \"all\". See ?optimizing.")

  if (is.null(stanModel))
    stanModel <- compile(spec, priorPredictive = FALSE, writeDir, ...)

  fun <- sprintf("optimizing_%s", keep)
  stanData <- make_data(spec, y, x, u, v)
  stanDots <- c(list(object = stanModel, data = stanData), list(...))
  stanOptimizing <- do.call(fun, list(stanDots = stanDots, nRuns = nRuns, nCores = nCores))
  attr(stanOptimizing, "data")     <- stanData
  attr(stanOptimizing, "filename") <- attr(stanModel, "filename")
  attr(stanOptimizing, "spec")     <- spec

  return(stanOptimizing)
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

  stanFit <- do.call(rstan::stan, stanDots)
  attr(stanFit, "data")     <- stanData
  attr(stanFit, "filename") <- stanFile
  attr(stanFit, "spec")     <- spec

  return(stanFit)
}

fit.Specification <- function(spec, y, x = NULL, u = NULL, v = NULL, ...) {
  run(spec, data = make_data(spec, y, x, u, v), ...)
}

sim.Specification <- function(spec, T = 1000, x = NULL, u = NULL, v = NULL, nSimulations = 500, ...) {
  dots <- list(...)
  dots[["spec"]] = spec
  dots[["data"]] = data = make_data(spec, y = NULL, x, u, v, T)
  dots[["iter"]] = nSimulations
  do.call(run, dots)
  # run(spec, data = make_data(spec, y = NULL, x, u, v, T), ...)
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

browse_model.Specification <- function(spec) {
  browseURL(write_model(spec, noLogLike = FALSE, writeDir = tempdir()))
}
