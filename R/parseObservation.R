parse_observation_build_priors <- function(observation, K, R) {
  # We're sure to get a KxR list here.
  # i.e. obsList[[k]][[r]] is always valid
  obsList <- observation
  for (k in 1:K) {
    for (r in 1:length(obsList[[k]])) { # not 1:R!
      lDensity <- obsList[[k]][[r]]
      lParam   <- getFreeParameters(lDensity)
      if (!is.empty(lParam)) {
        for (p in 1:length(lParam)) {
          nameParam <- names(lParam)[p]

          # Move down elements from observation density to parameters
          obsList[[k]][[r]][[nameParam]][["K"]]     <- K
          obsList[[k]][[r]][[nameParam]][["R"]]     <- R
          obsList[[k]][[r]][[nameParam]][["k"]]     <- lDensity$k
          obsList[[k]][[r]][[nameParam]][["r"]]     <- lDensity$r
          obsList[[k]][[r]][[nameParam]][["param"]] <- nameParam
          obsList[[k]][[r]][[nameParam]][["multivariate"]] <- is.multivariate(lDensity)

          # Move up elements from parameters to observation density
          if ("bounds" %in% names(obsList[[k]][[r]][[nameParam]])) {
            obsList[[k]][[r]][[paste0(nameParam, "Bounds")]] <- obsList[[k]][[r]][[nameParam]][["bounds"]]
          }
        }
      }
    }
  }
  obsList
}

# Add extra tags
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

    Rloop <-
      if (is.Density(observation)) {
        1
      } else if (is.DensityList(observation)) {
        length(observation)
      } else {
        stop("Oops?")
      }

    for (r in 1:Rloop) {
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

parse_observation <- function(observation, K, R) {
  if (is.null(observation)) {
    stop("Specification error: You must set an observation model. Please, read ?spec.")
  }

  msgerr <- function() {
    stop("Specification error: invalid number of densities in the observation model. Please see ?hmm for more information.")
  }

  # R   | Length  | Density             | FUN
  # --------------------------------------------------
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
  obsList <- parse_observation_build_priors(obsList, K, R)
  obsList
}
