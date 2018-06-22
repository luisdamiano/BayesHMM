parse_observation2 <- function(observation, K, R) {
  if (is.null(observation)) {
    stop("Specification error: You must set an observation model. Please, read ?spec.")
  }

  msgerr <- function() {
    stop("Specification error: invalid number of densities. Please see ?hmm for more information.")
  }

  #
  # R   | Length  | Density             | FUN
  #  1  | 1       | UnivariateDensity   | repeat_K
  #  1  | K       | UnivariateDensity   | repeat_none
  # >1  | 1       | MultivariateDensity | repeat_K
  # >1  | 1       | UnivariateDensity   | repeat_KxR
  # >1  | K       | MultivariateDensity | repeat_none
  # >1  | K       | UnivariateDensity   | repeat_R
  # >1  | R       | UnivariateDensity   | repeat_K
  # >1  | KxR     | UnivariateDensity   | repeat_none

  FUN <- ""
  if (R == 1) {
    len <- length(observation)
    if (
      is.DensityList(observation) &
      !is.multivariate(observation) &
      len == K
    ) {
      FUN <- "none"
    } else if (is.Density(observation)) {
      FUN <- "K"
    } else {
      msgerr()
    }
  } else {
    if (is.Density(observation)) {
      if ( is.multivariate(observation)) {  FUN <- "K"   }
      if (!is.multivariate(observation)) {  FUN <- "KxR" }
    } else if (is.DensityList(observation)) {
      len <- length(observation)
      mvd <- is.multivariate(observation)

      if (len == K) {
        if (mvd) { FUN <- "none" } else { FUN <- "R" }
      } else if (len == R) {
        if (mvd) { msgerr() } else { FUN <- "K" }
      } else if (len == K * R) {
        if (mvd) { msgerr() } else { FUN <- "none" }
      } else {
        msgerr()
      }
    } else {
      msgerr()
    }
  }

  FUN     <- match.fun(paste0("parse_observation_build_", FUN))
  obsList <- FUN(observation, K, R)
  # Expand priors
  # k state index; r output dimension index; p parameter index
  obsList <- parse_observation_build_priors(obsList, K, R)
  obsList
}

parse_observation_build_priors <- function(observation, K, R) {
  obsList <- observation
  for (k in 1:K) {
    for (r in 1:R) {
      lDensity <- obsList[[k]][[r]]
      lParam   <- getFreeParameters(lDensity)
      if (is.DensityList(lParam)) {
        for (p in 1:length(lParam)) {
          pName <- names(lParam)[p]

          # Move useful information down from Density to prior
          obsList[[k]][[pName]][["K"]]     <- K
          obsList[[k]][[pName]][["R"]]     <- R
          obsList[[k]][[pName]][["k"]]     <- lDensity$k
          obsList[[k]][[pName]][["r"]]     <- lDensity$r
          obsList[[k]][[pName]][["param"]] <- pName
          obsList[[k]][[pName]][["multivariate"]] <- is.multivariate(lDensity)

          # Move useful information up to Density from prior
          if ("bounds" %in% names(obsList[[k]][[r]][[pName]])) {
            obsList[[k]][[r]][[paste0(pName, "Bounds")]] <-
              obsList[[k]][[r]][[pName]][["bounds"]]
          }
        }
      }
    }
  }
  obsList
}

# Only adds extra tags.
parse_observation_build_none <- function(observation, K, R) {
  obsList <- list()
  for (k in 1:K) {
    kList <- list()
    kName <- paste0("k", k)
    rList <- observation[[k]]
    rList[["K"]] <- K
    rList[["R"]] <- R
    rList[["k"]] <- k
    rList[["r"]] <- 1
    rName <- paste0(kName, "r")
    kList[[rName]] <- rList
    obsList[[kName]] <- kList
  }
  obsList
}

# Take K densities in and repeat R times
parse_observation_build_R <- function(observation, K, R) {
  obsList <- list()
  for (k in 1:K) {
    kList <- list()
    kName <- paste0("k", k)
    for (r in 1:R) {
      rList <- observation[[k]]
      rList[["K"]] <- K
      rList[["R"]] <- R
      rList[["k"]] <- k
      rList[["r"]] <- r
      rName <- paste0(kName, "r", r)
      kList[[rName]] <- rList
    }
    obsList[[kName]] <- kList
  }
  obsList
}

# Take 1 or R densities in and repeat K times
parse_observation_build_K <- function(observation, K, R) {
  obsList <- list()
  for (k in 1:K) {
    kList <- list()
    kName <- paste0("k", k)
    for (r in 1:R) {
      rList <-
        if (is.DensityList(observation)) { observation[[r]] } else { observation }
      rList[["K"]] <- K
      rList[["R"]] <- R
      rList[["k"]] <- k
      rList[["r"]] <- r
      rName <- paste0(kName, "r", r)
      kList[[rName]] <- rList
    }
    obsList[[kName]] <- kList
  }
  obsList
}

# Take 1 density in and repeat K x R times
parse_observation_build_KxR <- function(observation, K, R) {
  obsList <- list()
  for (k in 1:K) {
    kList <- list()
    kName <- paste0("k", k)
    for (r in 1:R) {
      rList <- observation
      rList[["K"]] <- K
      rList[["R"]] <- R
      rList[["k"]] <- k
      rList[["r"]] <- r
      rName <- paste0(kName, "r", r)
      kList[[rName]] <- rList
    }
    obsList[[kName]] <- kList
  }
  obsList
}

# parse_observation is messy. It works but it's hard to read and there' are some redundancies's redundant code.
# We'll break it down in smaller pieces later.
parse_observation <- function(observation, K, R) {
  if (is.null(observation)) {
    stop("You must provide with an observation model. Please, read ?hmm.")
  }

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
      if (length(lParam) > 0) {
        for (p in 1:length(lParam)) {
          nameParam <- names(lParam)[p]
          if (is.Density(obsList[[k]][[nameParam]])) {
            # Move down elements from parent to child
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
        }
      }
    } else {
      for (r in 1:length(obsList[[k]])) {
        lDensity <- obsList[[k]][[r]]
        lParam   <- getFreeParameters(lDensity)
        if (length(lParam) > 0) {
          for (p in 1:length(lParam)) {
            nameParam <- names(lParam)[p]

            # Move down elements from parent to child
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
  }
  obsList
}

parse_initial <- function(initial, K, R) {
  if (is.null(initial)) { return(list()) }

  initList <- list()
  if (is.link(initial)) {
    kList <- initial
    kList[["k"]] <- ""
    kList[["r"]] <- ""
    kList[["param"]] <- "pi"
    kName <- paste0("k")
    initList[[kName]] <- kList
  } else if (is.Density(initial)) {
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

parse_transition <- function(transition, K, R) {
  if (is.null(transition)) { return(list()) }

  transList <- list()
  if (is.link(transition)) {
    kList <- transition
    kList[["k"]] <- ""
    kList[["r"]] <- ""
    kList[["param"]] <- "pi"
    kName <- paste0("k")
    transList[[kName]] <- kList
  } else if (is.Density(transition)) {
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

  # transList <- priors(transList, K, R)

  transList
}
