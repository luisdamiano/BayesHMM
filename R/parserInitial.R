# Take 1 density in and repeat K times
parse_initial_build_K <- function(initial, K, R) {
  initList <- list()
  for (k in 1:K) {
    kList <- initial
    kList[["k"]] <- paste0("", k) # sprintf("[%s]", k)
    kList[["r"]] <- ""
    kList[["param"]] <- "pi"
    kName <- paste0("k", k)
    initList[[kName]] <- kList
  }
  initList
}

# Take 1 multivariate density in and add extra tags
parse_initial_build_none_multivariate <- function(initial, K, R) {
  initList <- list()

  kList <- initial
  kList[["k"]] <- ""
  kList[["r"]] <- ""
  kList[["param"]] <- "pi"
  kName <- paste0("k")
  initList[[kName]] <- kList

  initList
}

# Take K univariate densities in and add extra tags
parse_initial_build_none_univariate <- function(initial, K, R) {
  initList <- list()
  for (k in 1:K) {
    if (is.multivariate(initial[[k]])) {
      stop("Specification error: when you set K priors for the K-sized vector of the initial distribution probabilities, the K priors must be univariate.")
    }

    kList <- initial[[k]]
    kList[["k"]] <- paste0("", k)
    kList[["r"]] <- ""
    kList[["param"]] <- "pi"
    kName <- paste0("k", k)
    initList[[kName]] <- kList
  }
  initList
}

parse_initial <- function(initial, K, R) {
  if (is.null(initial)) {
    stop("Specification error: You must set an initial model. Please, read ?spec.")
  }

  msgerr <- function() {
    stop("Specification error: invalid number of densities in the initial model. Please see ?hmm for more information.")
  }

  # Length  | Density             | FUN
  # ---------------------------------------------------------
  # 1       | UnivariateDensity   | repeat_K
  # 1       | MultivariateDensity | repeat_none_multivariate
  # 1       | LinkDensity         | repeat_none_multivariate
  # K       | UnivariateDensity   | repeat_none_univariate

  FUN <- ""
  if (is.Density(initial)) {
    if (is.multivariate(initial) | is.link(initial)) {
      FUN <- "none_multivariate"
    } else {
      FUN <- "K"
    }
  } else {
    if (is.DensityList(initial)) {
      if (length(initial) == K) {
        if (!is.multivariate(initial)) {
          FUN <- "none_univariate"
        } else {
          msgerr()
        }
      } else {
        msgerr()
      }
    }
  }

  FUN      <- match.fun(paste0("parse_initial_build_", FUN))
  initList <- FUN(initial, K, R)
  # initList <- parse_initial_build_priors(initList, K, R)
  initList
}
