check         <- function(x, ...) { UseMethod("check", x) }
fit           <- function(x, ...) { UseMethod("fit", x) }
write_chunks  <- function(x, ...) { UseMethod("write_chunks", x) }
write_model   <- function(x, ...) { UseMethod("write_model", x) }

hmm <- function(K, R, observation = NULL, initial = NULL,
                transition = NULL, name = "") {
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

  spec <- structure(l, class = "Specification")

  check(spec)

  spec
}

check.Specification <- function(spec) {
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

explain.Specification <- function(spec) {
  stop("TO BE IMPLEMENTED.")
}

is.multivariate.Specification <- function(spec) {
  all(densityApply(spec$observation$density, is.multivariate))
}

is.discrete.Specification <- function(spec) {
  all(densityApply(spec$observation$density, is.discrete))
}

# if data = NULL, then (prior predictive) generative model
fit.Specification <- function(spec, y = NULL, control = NULL,
                              writeDir = tempdir(), ...) {
  stanFile <- write_model(spec, noLogLike = is.null(y), writeDir)

  dots <- list(...)
  stanData <-
    if (is.null(y)) {
      if ("T" %in% (names(dots))) {
        T <- dots[["T"]]
        dots[["T"]] <- NULL
        list(T = T)
      } else {
        list(T = 1E3)
      }
    } else {
      list(
        y = y,
        T = NROW(y)
      )
    }

  stanDots <- c(
    dots,
    list(
      file       = stanFile,
      data       = stanData,
      model_name = spec$name
      ),
    control
  )

  fit <- do.call(rstan::stan, stanDots)
  attr(fit, "BayesHMM.filename") <- stanFile

  return(fit)
}

setGeneric(
  "stan_file",
   function(object) {
     attr(object, "BayesHMM.filename")
   }
)

methods::setGeneric("stan_file")

methods::setMethod("stan_file", signature(object = "stanfit"), function(object) {
  attr(object, "BayesHMM.filename")
})
