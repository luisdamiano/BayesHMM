# Take 1 density in and repeat K x R times
parse_transition_build_KxK <- function(transition, K, R) {
  transList <- list()
  for (i in 1:K) { # from i
    iList <- list()
    iName <- paste0("i", i)
    for (j in 1:K) { # to j
      jList <- transition
      jList[["K"]] <- K
      jList[["k"]] <- sprintf("[%s, %s]", i, j)
      jList[["multivariate"]] <- FALSE
      jList[["r"]] <- ""
      jList[["param"]] <- "A"
      jName <- paste0(iName, "j", j)
      iList[[jName]] <- jList
    }
    transList[[iName]] <- iList
  }
  transList
}

# Take 1 or K densities in and repeat K times
parse_transition_build_K <- function(transition, K, R) {
  transList <- list()
  for (i in 1:K) { # from i
    iList <- list()
    iName <- paste0("i", i)

    jLoop <-
      if (is.Density(transition)) {
        1
      } else if (is.DensityList(transition)) {
        length(transition)
      } else {
        stop("Oops?")
      }

    for (j in 1:jLoop) { # to j
      jList <-
        if (is.DensityList(transition)) { transition[[j]] } else { transition }
      jList[["K"]] <- K
      jList[["k"]] <-
        if (is.DensityList(transition)) { sprintf("[%s, %s]", i, j) } else { sprintf("[%s, ]", i) }
      jList[["r"]] <- ""
      jList[["param"]] <- "A"
      jName <- paste0(iName, "j", j)
      iList[[jName]] <- jList
    }

    transList[[iName]] <- iList
  }
  transList
}

# Take K multivariate density in and add extra tags
parse_transition_build_none_multivariate <- function(transition, K, R) {
  transList <- list()
  for (i in 1:K) {
    # if (is.multivariate(transition[[i]])) {
    #   stop("Specification error: when you set K priors for the K-sized vector of the transition distribution probabilities, the K priors must be univariate.")
    # }

    iList <- transition[[i]]
    iList[["k"]] <- sprintf("[%s, ]", i)
    iList[["r"]] <- ""
    iList[["param"]] <- "A"
    iName <- paste0("i", i)
    transList[[iName]] <- iList
  }
  transList
}

# Take KxK univariate density in and add extra tags
parse_transition_build_none_univariate <- function(transition, K, R) {
  transList <- list()
  for (i in 1:K) { # from i
    iList <- list()
    iName <- paste0("i", i)
    for (j in 1:K) { # to j
      jList <- transition[[(i - 1)*K + j]]
      jList[["K"]] <- K
      jList[["k"]] <- sprintf("[%s, %s]", i, j)
      jList[["r"]] <- ""
      jList[["param"]] <- "A"
      jName <- paste0(iName, "j", j)
      iList[[jName]] <- jList
    }
    transList[[iName]] <- iList
  }
  transList
}

parse_transition <- function(transition, K, R) {
  if (is.null(transition)) {
    return(NULL)
    # stop("Specification error: You must set a transition model. Please, read ?spec.")
  }

  msgerr <- function() {
    stop("Specification error: invalid number of densities in the transition model. Please see ?hmm for more information.")
  }

  # Length  | Density             | FUN
  # ---------------------------------------------------------
  # 1       | UnivariateDensity   | repeat_KxK
  # 1       | MultivariateDensity | repeat_K
  # 1       | LinkDensity         | ??
  # K       | UnivariateDensity   | repeat_K
  # K       | MultivariateDensity | repeat_none_multivariate
  # KxK     | UnivariateDensity   | repeat_none_univariate

  FUN <- ""
  if (is.Density(transition)) {
    if (is.multivariate(transition)) {
      FUN <- "K"
    } else {
      FUN <- "KxK"
    }
  } else {
    if (is.DensityList(transition)) {
      if (length(transition) == K) {
        if (is.multivariate(transition)) {
          FUN <- "none_multivariate"
        } else {
          FUN <- "K"
        }
      } else if (length(transition) == K*K & !is.multivariate(transition)) {
        FUN <- "none_univariate"
      } else {
        msgerr()
      }
    }
  }

  FUN      <- match.fun(paste0("parse_transition_build_", FUN))
  transList <- FUN(transition, K, R)
  # transList <- parse_transition_build_priors(transList, K, R)
  transList
}
