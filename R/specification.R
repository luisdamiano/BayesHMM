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

hmm <- function(K, R, observation = NULL, initial = NULL, transition = NULL) {
  # Observation model: 1, K x 1, K x R
  #   1 or K elements
  #     1 or R elements

  obsList <- list()
  if (is.Density(observation)) {
    # Case 1: only one density given.
    # Action: repeat the density for each one of the R dimensions in each one of the K states
    for (k in 1:K) {
      kList <- list()
      kName <- paste0("k", k)
      for (r in 1:R) {
        # Check if the density is multivariate -- if it is, don't repeat
        rList <- observation
        rName <- paste0(kName, "r", r)
        kList[[rName]] <- rList
      }
      obsList[[kName]] <- kList
    }
  } else {
    # Case 2: K densities given.
    for (k in 1:K) {
      if (length(observation) != K) {
        stop(
          sprintf("I received %s densities. Expected 1 or K = %s.", length(observation), K)
        )
      }
      kList <- list() # observation[[k]]
      kName <- paste0("k", k)

      # Case 2a: K univariate densities given.
      # Action : Repeat Density for each R dimension.
      if (is.Density(observation[[k]])) { # R = 1
        for (r in 1:R) {
          rList <- observation[[k]]
          rName <- paste0(kName, "r", r)
          kList[[rName]] <- rList
        }
      } else {
      # Case 2b: K univariate densities with R densities in each.
      # Action : Direct assignment.
        for (r in 1:R) {
          stop("TO BE IMPLEMENTED")
          rList <- observation[[k]][[r]]
          rName <- paste0(kName, "r", r)
          kList[[rName]] <- rList
        }
      }

      obsList[[kName]] <- kList
    }
  }

  initList <- list()
  if (is.Density(initial)) {
    # Case 1: One multivariate density.
    # Action: Repeat the univariate density for each state K.
    if (is.multivariate(initial)) {
      kList <- initial # remember to change the k and r arguments accordingly
      kName <- paste0("k")
      initList[[kName]] <- kList
    } else {
    # Case 2: One univariate density.
    # Action: Repeat the univariate density for each state K.
      for (k in 1:K) {
        kList <- initial # remember to change the k and r arguments accordingly
        kName <- paste0("k", k)
        initList[[kName]] <- kList
      }
    }
  } else {
    if (length(initial) != K) {
      stop(
        sprintf("I received %s densities. Expected 1 or K = %s.", length(initial), K)
      )
    }
    # Case 3: K multivariate density (one per each element pi_k).
    # Action: Direct assignment.
    for (k in 1:K) {
      if (is.multivariate(initial[[k]])) {
        stop("When you set K priors for the K-sized vector of the initial distribution probabilities, the K priors must be univariate.")
      }

      kList <- initial[[k]] # remember to change the k and r arguments accordingly
      kName <- paste0("k", k)
      initList[[kName]] <- kList
    }
  }

  transList <- list()
  if (is.Density(transition)) {
    if (is.multivariate(transition)) {
      # Case 1: only one multivariate density given.
      # Action: repeat the density for each row in the transition matrix
      for (k in 1:K) {
        kList <- list()
        kName <- paste0("k", k)

        kkList <- transition # remember to change the k and r arguments accordingly
        kkName <- paste0(kName, "k")
        kList[[kkName]] <- kkList

        transList[[kName]] <- kList
      }
    } else {
      # Case 1: only one univariate density given.
      # Action: repeat the density for each one of the KxK elements in the transition matrix
      for (k in 1:K) {
        kList <- list()
        kName <- paste0("k", k)
        for (k in 1:K) {
          kkList <- transition # remember to change the k and r arguments accordingly
          kkName <- paste0(kName, "k", k)
          kList[[kkName]] <- kkList
        }
        transList[[kName]] <- kList
      }
    }
  } else {
    if (length(transition) != K) {
      stop(
        sprintf("I received %s densities. Expected 1 or K = %s.", length(transition), K)
      )
    }

    for (k in 1:K) {
      if (is.multivariate(transition[[k]])) {
        # Case 1: only one multivariate density given.
        # Action: repeat the density for each row in the transition matrix
        kList <- list()
        kName <- paste0("k", k)

        kkList <- transition[[k]] # remember to change the k and r arguments accordingly
        kkName <- paste0(kName, "k")
        kList[[kkName]] <- kkList

        transList[[kName]] <- kList
      } else {
        # Case 1: only one univariate density given.
        # Action: repeat the density for each one of the KxK elements in the transition matrix
        kList <- list()
        kName <- paste0("k", k)
        for (kk in 1:K) {
          kkList <- transition[[k]] # remember to change the k and r arguments accordingly
          kkName <- paste0(kName, "k", kk)
          kList[[kkName]] <- kkList
        }
        transList[[kName]] <- kList
      }
    }
    print("Does it make sense to have different priors for the transition matrix in different states?")
  }

  l <- list(
    name = "",
    K    = K,
    observation = list(
      R = 1,
      covariates = NULL,
      density = observation
    ),
    init_prob   = list(
      density = initial
    ),
    transition  = list(
      covariates = NULL,
      density = list(
        k1 = transition
      )
    )
  )
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
  base  <- file.path("inst", "stan", sprintf("%s-%s.stan", baseR, baseA))

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
