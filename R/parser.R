parse_observation <- function(observation, K, R) {
  obsList <- list()
  if (is.Density(observation)) {
    if (is.multivariate(observation)) {
      # Case 1.a: one multivariate density given.
      # Action  : repeat the density for each one of the K states
      for (k in 1:K) {
        kList <- observation
        kList[["k"]] <- k
        kList[["r"]] <- 1
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
          rList[["r"]] <- r
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
          rList[["r"]] <- 1
          rName <- paste0(kName, "r")
          kList[[rName]] <- rList
        } else {
          for (r in 1:R) {
            rList <- observation[[k]]
            rList[["k"]] <- k
            rList[["r"]] <- r
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
    if (is.Density(obsList[[k]])) {
      lDensity <- obsList[[k]]
      lParam   <- getFreeParameters(lDensity)
      for (p in 1:length(lParam)) {
        # Move down elements from parent to child
        nameParam <- names(lParam)[p]
        obsList[[k]][[nameParam]][["K"]]     <- K
        obsList[[k]][[nameParam]][["R"]]     <- R
        obsList[[k]][[nameParam]][["k"]]     <- lDensity$k
        obsList[[k]][[nameParam]][["r"]]     <- lDensity$r
        obsList[[k]][[nameParam]][["param"]] <- nameParam
        obsList[[k]][[nameParam]][["multivariate"]] <- is.multivariate(lDensity)

        # Move down to nested elements from parent to grandchildren ^_^
        nestedParams <- (1:length(obsList[[k]][[nameParam]]))[sapply(obsList[[k]][[nameParam]], is.Density)]
        if (any(nestedParams)) {
          for (np in 1:length(nestedParams)) {
            nameNestedParam <- nestedParams[np]
            obsList[[k]][[nameParam]][[nameNestedParam]][["K"]]     <- K
            obsList[[k]][[nameParam]][[nameNestedParam]][["R"]]     <- R
            obsList[[k]][[nameParam]][[nameNestedParam]][["k"]]     <- lDensity$k
            obsList[[k]][[nameParam]][[nameNestedParam]][["r"]]     <- np
            obsList[[k]][[nameParam]][[nameNestedParam]][["param"]] <- nameParam
            obsList[[k]][[nameParam]][[nameNestedParam]][["multivariate"]] <- is.multivariate(lDensity)
          }
        }

        # Move up elements from child to parent
        if (!is.null(obsList[[k]][[nameParam]][["bounds"]])) {
          obsList[[k]][[paste0(nameParam, "Bounds")]] <- obsList[[k]][[nameParam]][["bounds"]]
        }
      }
    } else {
      for (r in 1:length(obsList[[k]])) {
        lDensity <- obsList[[k]][[r]]
        lParam   <- getFreeParameters(lDensity)
        for (p in 1:length(lParam)) {
          # Move down elements from parent to child
          nameParam <- names(lParam)[p]
          obsList[[k]][[r]][[nameParam]][["K"]]     <- K
          obsList[[k]][[r]][[nameParam]][["R"]]     <- R
          obsList[[k]][[r]][[nameParam]][["k"]]     <- lDensity$k
          obsList[[k]][[r]][[nameParam]][["r"]]     <- lDensity$r
          obsList[[k]][[r]][[nameParam]][["param"]] <- nameParam
          obsList[[k]][[r]][[nameParam]][["multivariate"]] <- is.multivariate(lDensity)

          # Move up elements from child to parent
          # if (!is.null(obsList[[k]][[r]][[nameParam]][["bounds"]])) {
          if ("bounds" %in% names(obsList[[k]][[r]][[nameParam]])) {
            obsList[[k]][[r]][[paste0(nameParam, "Bounds")]] <- obsList[[k]][[r]][[nameParam]][["bounds"]]
          }
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
