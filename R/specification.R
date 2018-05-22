check         <- function(x, ...) { UseMethod("check", x) }
write_chunks  <- function(x, ...) { UseMethod("write_chunks", x) }
write_model   <- function(x, ...) { UseMethod("write_model", x) }

parse_observation <- function(observation, K, R) {
  obsList <- list()
  if (is.Density(observation)) {
    if (is.multivariate(observation)) {
      # Case 1.a: one multivariate density given.
      # Action  : repeat the density for each one of the K states
      for (k in 1:K) {
        kList <- observation # remember to change the k and r arguments accordingly
        kList[["k"]] <- k
        kList[["r"]] <- ""
        kName <- paste0("k", k)
        obsList[[kName]] <- kList
      }
    } else {
      for (k in 1:K) {
        # Case 1.b: one univaraite density given.
        # Action  : repeat the density for each one of the R dimensions in each one of the K states
        kList <- list()
        kName <- paste0("k", k)
        for (r in 1:R) {
          # Check if the density is multivariate -- if it is, don't repeat
          rList <- observation
          rList[["k"]] <- k
          rList[["r"]] <- if (R == 1) { "" } else { r }
          rName <- paste0(kName, "r", r)
          kList[[rName]] <- rList
        }
        obsList[[kName]] <- kList
      }
    }
  } else {
    # Case 2: K densities given.
    for (k in 1:K) {
      if (length(observation) != K) {
        stop(
          sprintf("I received %s densities. Expected 1 or K = %s.", length(observation), K)
        )
      }

      kList <- list()
      kName <- paste0("k", k)
      # Case 2a: K univariate densities given.
      # Action : Repeat Density for each R dimension.
      if (is.Density(observation[[k]])) {
        if (is.multivariate(observation[[k]])) {
          rList <- observation[[k]]
          rList[["k"]] <- k
          rList[["r"]] <- ""
          rName <- paste0(kName, "r")
          kList[[rName]] <- rList
        } else {
          for (r in 1:R) {
            rList <- observation[[k]]
            rList[["k"]] <- k
            rList[["r"]] <- "" ### univariate: r = 1 or no r?
            rName <- paste0(kName, "r", r)
            kList[[rName]] <- rList
          }
        }
      } else {
        # Case 2b: K univariate densities with R densities in each.
        # Action : Direct assignment.
        stop("TO BE IMPLEMENTED (check if this makes sense first).")
      }

      obsList[[kName]] <- kList
    }
  }

  # Expand priors
  # k state index; r output dimension index; p parameter index
  for (k in 1:length(obsList)) {
    for (r in 1:length(obsList[[k]])) {
      lDensity <- obsList[[k]][[r]]
      lParam   <- getParameters(lDensity)
      for (p in 1:length(lParam)) {
        # Move down elements from parent to child
        nameParam <- names(lParam)[p]
        obsList[[k]][[r]][[nameParam]][["k"]]     <- lDensity$k
        obsList[[k]][[r]][[nameParam]][["r"]]     <- lDensity$r # if (lDensity$r == "") { "" } else { lDensity$r }
        obsList[[k]][[r]][[nameParam]][["param"]] <- nameParam

        # Move up elements from child to parent
        if (!is.null(obsList[[k]][[r]][[nameParam]][["bounds"]])) {
          obsList[[k]][[r]][[paste0(nameParam, "Bounds")]] <- obsList[[k]][[r]][[nameParam]][["bounds"]]
        }
      }
    }
  }
  obsList
}

parse_initial <- function(initial, K) {
  initList <- list()
  if (is.Density(initial)) {
    # Case 1: One multivariate density.
    # Action: Repeat the univariate density for each state K.
    if (is.multivariate(initial)) {
      kList <- initial
      kList[["k"]] <- ""
      kList[["r"]] <- ""
      kList[["param"]] <- "pi"
      kName <- paste0("k")
      initList[[kName]] <- kList
    } else {
      # Case 2: One univariate density.
      # Action: Repeat the univariate density for each state K.
      for (k in 1:K) {
        kList <- initial
        kList[["k"]] <- sprintf("[%s]", k)
        kList[["r"]] <- ""
        kList[["param"]] <- "pi"
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
    # Case 3: K univariate densities (one per each element pi_k).
    # Action: Direct assignment.
    for (k in 1:K) {
      if (is.multivariate(initial[[k]])) {
        stop("When you set K priors for the K-sized vector of the initial distribution probabilities, the K priors must be univariate.")
      }

      kList <- initial[[k]]
      kList[["k"]] <- sprintf("[%s]", k)
      kList[["r"]] <- ""
      kList[["param"]] <- "pi"
      kName <- paste0("k", k)
      initList[[kName]] <- kList
    }
  }
  initList
}

parse_transition <- function(transition, K) {
  transList <- list()
  if (is.Density(transition)) {
    if (is.multivariate(transition)) {
      # Case 1: only one multivariate density given.
      # Action: repeat the density for each row in the transition matrix
      for (k in 1:K) {
        kList <- list()
        kName <- paste0("k", k)

        kkList <- transition # remember to change the k and r arguments accordingly
        kkList[["k"]] <- sprintf("[%s, ]", k)
        kkList[["r"]] <- ""
        kkList[["param"]] <- "A"
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
        for (kk in 1:K) {
          kkList <- transition # remember to change the k and r arguments accordingly
          kkName <- paste0(kName, "k", k, "k", kk)
          kkList[["k"]] <- sprintf("[%s, %s]", k, kk)
          kkList[["r"]] <- ""
          kkList[["param"]] <- "A"
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
        kkList[["k"]] <- sprintf("[%s, ]", k)
        kkList[["r"]] <- ""
        kkList[["param"]] <- "A"
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
          kkList[["k"]] <- sprintf("[%s, %s]", k, kk)
          kkList[["r"]] <- ""
          kkList[["param"]] <- "A"
          kkName <- paste0(kName, "k", kk)
          kList[[kkName]] <- kkList
        }
        transList[[kName]] <- kList
      }
    }
    print("Does it make sense to have different priors for the transition matrix in different states?")
  }

  transList
}

hmm <- function(K, R, observation = NULL, initial = NULL, transition = NULL, name = "") {
  # Observation model: 1, K x 1, K x R
  #   1 or K elements
  #     1 or R elements

  l <- list(
    name = name,
    K    = K,
    observation = list(
      R = R,
      covariates = NULL,
      density = parse_observation(observation, K, R)
    ),
    init_prob   = list(
      density = parse_initial(initial, K)
    ),
    transition  = list(
      covariates = NULL,
      density = parse_transition(transition, K)
    )
  )
  structure(l, class = "Specification")
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
  # base  <- system.file("stan", sprintf("%s-%s.stan", baseR, baseA), package = "BayesHMM")

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

# explain.Specification <- function(spec) {
#   sprintf("Here I'll explain my spec.")
# }
#
# explain_initial <- function(initial, K) {
#   tab <- lapply(1:K, function(k) {
#     data.frame(k, explain(initial[[k]]), stringsAsFactors = FALSE)
#   })
#   tab <- do.call(rbind, tab)
#   colnames(tab) <- c("K", "Density")
#   print(tab)
# }
#
# explain_transition <- function(transition, K) {
#   tab <- lapply(1:K, function(k) {
#     lapply(1:K, function(kk) {
#       data.frame(k, kk, explain(transition[[k]][[kk]]), stringsAsFactors = FALSE)
#     })
#   })
#   tab <- do.call(rbind, unlist(tab, recursive = FALSE))
#   colnames(tab) <- c("K (from)", "K (to)", "Density")
#   print(tab)
# }
#
# explain_observation <- function(observation) {
#   tab <-
#     lapply(observation, function(obsK) {
#       lapply(obsK, function(obsKR) {
#         data.frame(k, r, explain(obsKR), stringsAsFactors = FALSE)
#       })
#     }, K = length(observation))
#   tab <- do.call(rbind, unlist(tab, recursive = FALSE))
#   colnames(tab) <- c("K", "R", "Density")
#   print(tab)
# }
#
# explain.Specification <- function(x) {
#   explainInit  <- explain_initial(x$init_prob$density, x$K)
#   explainTrans <- explain_transition(x$transition$density, x$K)
#   explainObs   <- explain_observation(x$observation$density)
#
#   hl <- paste0(rep("#", 80), collapse = "")
#   sprintf("
#     %s
#     # MODEL: %s
#     %s
#
#     %s
#     # 1. Initial state distribution
#     %s
#     %s
#
#     %s
#     # 2. Transition model
#     %s
#     %s
#
#     %s
#     # 3. Observation model
#     %s
#     %s
#     ",
#     hl, x$name, hl,
#     hl, explainInit, hl,
#     hl, explainTrans, hl,
#     hl, explainObs, hl
#   )
# }
