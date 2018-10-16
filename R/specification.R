#' Create a model specification
#'
#' @param K An integer with the number of hidden states.
#' @param R An integer with the dimension of the observation vector (e.g. one is univariate, two is bivariate)
#' @param observation One density, or more than one density chained with the `+` operator, describing the observation model. See below and VIGNETTE for detailed explanation.
#' @param initial One density, or more than one density chained with the `+` operator, describing the initial distribution model. See below and VIGNETTE for detailed explanation.
#' @param transition One density, or more than one density chained with the `+` operator, describing the transition model. See below and VIGNETTE for detailed explanation.
#' @param name An optional string with a name for a model.
#' @return An specification object that may be used to generate data from or fit a model.
#' @section Model specification:
#' Explanation of model specification here.
#' @export
#' @examples
spec <- function(K, R, observation = NULL, initial = NULL,
                 transition = NULL, name = "") {
  check_natural(K, "K")
  check_natural(R, "R")

  K <- as.integer(K)
  R <- as.integer(R)

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

#' Verify that the object is a valid specification. TO BE IMPLEMENTED.
#'
#' This function verifies that the structure of the object is valid. Useful to spot inconsistencies in the specification. \strong{TO BE IMPLEMENTED}.
#' @usage check(spec)
#' @param spec An object returned by either \code{\link{spec}} or \code{\link{hmm}}.
#' @return A logical value with TRUE if the object is a valid specification or FALSE otherwise.
#' @export
#' @examples
check             <- function(spec, ...) { UseMethod("check", spec) }

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

#' Create an user-friendly text describing the model.
#'
#' The function creates a user-friendly text describing any of the three elements of the model. It includes the hidden states, variables, densities, bounds, priors, and fixed parameters. It also records environment details for easier reproducibility (package version, R version, time, OS).
#'
#' @usage explain(spec, observation = TRUE, initial = TRUE, transition = TRUE, print = TRUE)
#' @param spec An object returned by either \code{\link{spec}} or \code{\link{hmm}}.
#' @param observation An optional logical indicating whether the observation model should be included in the description. It defaults to TRUE.
#' @param initial An optional logical indicating whether the initial distribution model should be included in the description. It defaults to TRUE.
#' @param transition An optional logical indicating whether the transition model should be included in the description. It defaults to TRUE.
#' @param print An optional logical indicating whether the description should be printing out.
#' @return A character string with the model description.
#' @export
#'
#' @examples
explain           <- function(spec, ...) { UseMethod("explain", spec) }

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

#' Create an outline of the observation model.
#'
#' @usage explain_observation(spec)
#' @param spec An object returned by the \code{\link{spec}}) function.
#' @return A character vector with an outline of the observation model.
#' @family explain
#' @export
#' @keywords internal
#' @examples
explain_observation <- function(spec, ...) { UseMethod("explain_observation", spec) }

explain_observation.Specification <- function(spec) {
  R <- spec$observation$R

  block1 <-
    sprintf(
      "%s observations (R = %d): %s.\n",
      if (R > 1) { "Multivariate" } else { "Univariate" },
      R, "Variable names"
    )

  l <- densityApply(spec$observation$density, explain)

  block2 <-
    if (all(sapply(l, identical, l[[1]]))) {
      sprintf(
        "Observation model for all states\n%s\n", l[[1]]
      )
    } else {
      k <- sub("k[[:digit:]].k([[:digit:]])r[[:digit:]]", "\\1", names(l))
      r <- sub("k[[:digit:]].k[[:digit:]]r([[:digit:]])", "\\1", names(l))
      sprintf(
        "\nObservation model for State %s and Variable %s\n%s\n",
        k, r, l
      )
    }

  collapse(c(block1, block2))
}

#' Create an outline of the initial model.
#'
#' @usage explain_initial(spec)
#' @param spec An object returned by the \code{\link{spec}}) function.
#' @return A character vector with an outline of the initial model.
#' @family explain
#' @export
#' @keywords internal
#' @examples
explain_initial     <- function(spec, ...) { UseMethod("explain_initial", spec) }

explain_initial.Specification <- function(spec) {
  l <- densityApply(spec$initial$density, explain)

  block1 <-
    if (all(sapply(l, identical, l[[1]]))) {
      sprintf(
        "Initial distribution model\n%s\n", l[[1]]
      )
    } else {
      k <- sub("i[[:digit:]].i([[:digit:]])j[[:digit:]]", "\\1", names(l))
      sprintf(
        "\nInitial probability for State %s\n%s\n",
        k, l
      )
    }

  collapse(block1)
}

#' Create an outline of the transition model.
#'
#' @usage explain_transition(spec)
#' @param spec An object returned by the \code{\link{spec}}) function.
#' @return A character vector with an outline of the transition model.
#' @family explain
#' @export
#' @keywords internal
#' @examples
explain_transition  <- function(spec, ...) { UseMethod("explain_transition", spec) }

explain_transition.Specification <- function(spec) {
  l <- densityApply(spec$transition$density, explain)

  block1 <-
    if (all(sapply(l, identical, l[[1]]))) {
      sprintf(
        "Transition model\n%s\n", l[[1]]
      )
    } else {
      i <- sub("i[[:digit:]].i([[:digit:]])j[[:digit:]]", "\\1", names(l))
      j <- sub("i[[:digit:]].i[[:digit:]]j([[:digit:]])", "\\1", names(l))
      sprintf(
        "\nTransition probability from State %s to State %s\n%s\n",
        i, j, l
      )
    }

  collapse(block1)
}

#' Compile a specified model.
#'
#' This function turns the model specification into Stan code and compiles the program via rstan.
#'
#' @usage compile(spec, priorPredictive = FALSE, writeDir = tempdir(), ...)
#' @param spec An object returned by either \code{\link{spec}} or \code{\link{hmm}}.
#' @param priorPredictive An optional logical stating whether the log-likelihood should be excluded from the program. If TRUE, the returned object can only be used to draw samples from the prior predictive density. If FALSE, the returned object can only be used to draw samples from the posterior predictive density. It defaults to FALSE.
#' @param writeDir An optional string with the path where the Stan file should be written. Useful to inspect and modify the Stan code manually. It defaults to a temporary directory.
#' @param ... Arguments to be passed to rstan's \code{\link[rstan]{stan_model}}.
#' @return An instance of S4 class stanmodel.
#' @export
#' @examples
compile           <- function(spec, ...) { UseMethod("compile", spec) }

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

# Full-Bayesian estimation ------------------------------------------------

#' Draw samples from a specification.
#'
#' @usage function(spec, stanModel = NULL, y, x = NULL, u = NULL, v = NULL, writeDir = tempdir(), ...)
#' @param spec An object returned by either \code{\link{spec}} or \code{\link{hmm}}.
#' @param stanModel An optional instance of S4 class stanmodel returned by \code{\link{compile}}. If not given, the model is automatically compiled but the object is not returned to the user and cannot be reutilized in future sampling.
#' @param y A numeric matrix with the observation sample. It must have as many rows as the time series length \emph{T} and as many columns as the dimension of the observation vector \emph{R}. If not a matrix, the function tries to cast the object to a \eqn{T\times R} matrix.
#' @param x An optional numeric matrix with the covariates for the observation model. It must have as many rows as the time series length \emph{T} and as many columns as the dimension of the covariate vector \emph{M}. If not a matrix, the function tries to cast the object to a \eqn{T\times M} matrix. Useful for Hidden Markov Regression Model (also known as Markov-switching regressions).
#' @param u An optional numeric matrix with the covariates for the transition model. It must have as many rows as the time series length \emph{T} and as many columns as the dimension of the transition covariate vector \emph{P}. If not a matrix, the function tries to cast the object to a \eqn{T\times P} matrix. Useful for Hidden Markov Models with time-varying transition probabilities.
#' @param v An optional numeric matrix with the covariates for the initial distribution model. It must have as many rows as the number of hidden states \emph{K} and as many columns as the dimension of the initial covariate vector \emph{Q}. If not a matrix, the function tries to cast the object to a \eqn{K\times Q} matrix.
#' @param writeDir An optional string with the path where the Stan file should be written. Useful to inspect and modify the Stan code manually. It defaults to a temporary directory.
#' @param ... Arguments to be passed to rstan's \code{\link[rstan]{sampling}}.
#' @return An object of S4 class stanfit with some additional attributes (the dataset \emph{data}, the name of the Stan code file \emph{filename}, and the specification object \emph{spec}). This object is completely compatible with all other functions.
#' @seealso See rstan's \code{\link[rstan]{stan}} and \code{\link[rstan]{sampling}} for further details on tunning the MCMC algorithm.
#' @export
#' @examples
sampling          <- function(spec, ...) { UseMethod("sampling", spec) }

sampling.Specification <- function(spec, stanModel = NULL, y, x = NULL, u = NULL, v = NULL,
                                   writeDir = tempdir(), ...) {

  if (is.null(stanModel)) {
    stanModel <- compile(spec, priorPredictive = FALSE, writeDir)
  }

  stanData <- make_data(spec, y, x, u, v)
  stanDots <- c(list(...), list(object = stanModel, data = stanData))

  stanSampling <- do.call(rstan::sampling, stanDots)
  attr(stanSampling, "data")     <- stanData
  attr(stanSampling, "filename") <- attr(stanModel, "filename")
  attr(stanSampling, "spec")     <- spec

  return(stanSampling)
}

#' Run a Markov-chain Monte Carlo algorithm to sample from the log posterior density.
#'
#' @usage run(spec, data = NULL, writeDir = tempdir(), ...)
#' @inherit sampling
#' @param ... Arguments to be passed to rstan's \code{\link[rstan]{stan}}.
#' @keywords internal
#' @export
run               <- function(spec, ...) { UseMethod("run", spec) }

run.Specification <- function(spec, data = NULL, writeDir = tempdir(), ...) {

  stanData <- data
  stanFile <- write_model(spec, noLogLike = is.null(data$y), writeDir)

  stanDots <- c(
    list(...),
    list(
      file       = stanFile,
      data       = stanData,
      model_name = spec$name
    )
  )

  stanFit <- do.call(rstan::stan, stanDots)
  attr(stanFit, "data")     <- stanData
  attr(stanFit, "filename") <- stanFile
  attr(stanFit, "spec")     <- spec

  return(stanFit)
}

#' Fit a model by MCMC
#'
#' @usage fit(spec, y, x = NULL, u = NULL, v = NULL, ...)
#' @inherit sampling
#' @param ... Arguments to be passed to rstan's \code{\link[rstan]{stan}}.
#' @export
#' @examples
fit               <- function(spec, ...) { UseMethod("fit", spec) }

fit.Specification <- function(spec, y, x = NULL, u = NULL, v = NULL, ...) {
  run(spec, data = make_data(spec, y, x, u, v), ...)
}

# Maximum a posteriori estimation -----------------------------------------

#' Fit a model by MAP
#'
#' This function computes a maximum a posteriori estimate by running one or more instances of a numerical optimization procedure to maximize the joint posterior density. If no seed is given, one is automatically generated and stored as an attribute in the returned object. An error is printed if no convergence was achieved after all the runs.
#'
#' @usage optimizing(spec, stanModel = NULL, y, x = NULL, u = NULL, v = NULL, nRuns = 1, keep = "best", nCores = 1, writeDir = tempdir(), ...)
#' @inheritParams sampling
#' @param nRuns An optional integer with the number of initializations.
#' @param keep An optional string specifying whether the function should return the converging instance with the maximum posterior log density (\emph{best}) or all the instances (\emph{all}). The latter may be useful for debugging. It defaults to \emph{best}.
#' @param nCores An optional integer with the number of cores to be used. If equal to one, the instances are run sequentially. Otherwise, doParallel's backend is used for parallel computing. It defaults to one.
#' @param ... Arguments to be passed to rstan's \code{\link[rstan]{optimizing}}.
#' @return An \emph{Optimization} object if \emph{keep} is set to \emph{best}), or an \emph{OptimizationList} otherwise. In the latter case, the best instance can be obtained with \code{\link{extract_best}}.
#' @export
#' @seealso See \code{\link[rstan]{optimizing}} for further details on tunning the optimization procedure.
#' @examples
optimizing        <- function(spec, ...) { UseMethod("optimizing", spec) }

optimizing.Specification <- function(spec, stanModel = NULL, y, x = NULL, u = NULL, v = NULL,
                                     nRuns = 1, keep = "best", nCores = 1,
                                     writeDir = tempdir(), ...) {

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

#' Run one instance of the
#'
#' @param stanDots The arguments to the passed to rstan's \code{\link[rstan]{optimizing}}.
#' @param n An integer with the number of the instance (i.e. the n-th time the algorithm is run on this model).
#' @return An \epm{Optimization} object.
#' @export
#' @keywords internal
optimizing_run  <- function(stanDots, n) {
  # sink(tempfile())

  stanDots[["seed"]] <-
    if ("seed" %in% names(stanDots)) {
      as.integer(stanDots[["seed"]] + n)
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

#' Run several instances of the optimization algorithm.
#'
#' Note that this function returns the results of all the instances while \code{\link{optimizing_best}} only returns the converging instance with highest log posterior density.
#' @param stanDots The arguments to the passed to rstan's \code{\link[rstan]{optimizing}}.
#' @param nRuns An optional integer with the number of initializations.
#' @param nCores An optional integer with the number of cores to be used. If equal to one, the instances are run sequentially. Otherwise, doParallel's backend is used for parallel computing. It defaults to one.
#' @return An \epm{OptimizationList} object.
#' @export
#' @keywords internal
optimizing_all  <- function(stanDots, nRuns, nCores) {
  l <- if (nCores == 1) {
    lapply(seq_len(nRuns), function(n) optimizing_run(stanDots, n))
  } else {
    cl <- parallel::makeCluster(nCores, outfile = "")
    doParallel::registerDoParallel(cl)
    on.exit({parallel::stopCluster(cl)})
    `%dopar%` <- foreach:::`%dopar%`
    foreach::foreach(n = seq_len(nRuns), .combine = c, .packages = c("rstan")) %dopar% {
      optimizing_run(stanDots, n)
    }
  }
  l <- lapply(seq_len(nRuns), function(n) optimizing_run(stanDots, n))
  structure(l, class = c("OptimizationList", "list"))
}

#' Run several instances of the optimization algorithm.
#'
#' Note that this function returns the results of the converging instance with highest log posterior density while \code{\link{optimizing_all}} returns all.
#' @inherit optimizing_all
#' @return An \epm{Optimization} object.
#' @export
#' @keywords internal
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

# Other methods -----------------------------------------------------------
#' Simulate data from the prior predictive density.
#'
#' @usage sim(spec, T = 1000, x = NULL, u = NULL, v = NULL, nSimulations = 500, ...)
#' @inheritParams sampling
#' @param nSimulations An optional integer with the number of simulations. It defaults to 500 time series.
#' @return
#' @export
#' @examples
sim               <- function(spec, ...) { UseMethod("sim", spec) }

sim.Specification <- function(spec, T = 1000, x = NULL, u = NULL, v = NULL, nSimulations = 500, ...) {
  dots <- list(...)
  dots[["spec"]] <- spec
  dots[["data"]] <- make_data(spec, y = NULL, x, u, v, T)
  dots[["iter"]] <- nSimulations
  do.call(run, dots)
}

# Generic defined and documented in fitIntegration.R
browse_model.Specification <- function(spec) {
  browseURL(write_model(spec, noLogLike = FALSE, writeDir = tempdir()))
}
