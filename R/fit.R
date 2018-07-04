select_parameters <- function(fit, observation = TRUE,
                              initial = TRUE, transition = TRUE) {
  spec <- attr(fit, "spec")
  paramNames   <- ""

  if (observation) {
    paramNames  <- c(paramNames, densityApply(spec$observation$density, getParameterNames))
  }

  if (initial) {
    paramNames  <- c(paramNames, densityCollect(mySpec$initial$density, `[[`, "param"))
  }

  if (transition) {
    paramNames  <- c(paramNames, densityCollect(mySpec$transition$density, `[[`, "param"))
  }

  paramPatterns <- glob2rx(sprintf("%s*", unique(paramNames)[-1]))
  fitNames      <- names(extract(fit))
  paramInd <- Reduce(
    `|`,
    lapply(paramPatterns, function(pattern) {
      grepl(pattern, fitNames)
    })
  )

  fitNames[paramInd]
}

select_obs_parameters <- function(fit) {
  select_parameters(fit, TRUE, FALSE, FALSE)
}

extract_obs <- function(fit, ...) {
  extract(fit, pars = select_obs_parameters(fit), ...)
}

summary_obs <- function(fit, ...) {
  summary(fit, pars = select_obs_parameters(fit), ...)
}

plot_obs <- function(fit, ...) {
  plot(fit, pars = select_obs_parameters(fit), ...)
}

print_obs <- function(fit, ...) {
  print(fit, pars = select_obs_parameters(fit), ...)
}

browse_model <- function(fit) {
  browseURL(attr(fit, "filename"))
}

extract_quantity <- function(stanfit, pars, summary = NULL, ...) {
  x    <- extract(stanfit, pars = pars, ...)[[1]]
  dims <- length(dim(x))

  if (is.null(summary)) {
    return(x)
  }

  summaryFun <- function(x) {
    if (length(summary) == 1 && summary == "mean") {
      mean(x)
    } else {
      quantile(x, prob = summary)
    }
  }

  if (dims == 1) {
    return(summaryFun(x))
  } else {
    return(apply(x, seq.int(2, dims), summaryFun))
  }
}

extract_params <- function(stanfit, observation = TRUE, initial = TRUE, transition = TRUE, ...) {
  pars <- select_parameters(stanfit, observation, initial, transition)
  sapply(pars, function(p) { extract_quantity(stanfit, p, ...) })
}

extract_alpha <- function(stanfit, ...) {
  extract_quantity(stanfit, "alpha", ...)
}

extract_gamma <- function(stanfit, ...) {
  extract_quantity(stanfit, "gamma", ...)
}

extract_zstar <- function(stanfit, ...) {
  extract_quantity(stanfit, "zstar", ...)
}

extract_ypred <- function(stanfit, ...) {
  extract_quantity(stanfit, "ypred", ...)
}

extract_zpred <- function(stanfit, ...) {
  extract_quantity(stanfit, "zpred", ...)
}

extract_data <- function(stanfit) {
  attr(stanfit, "data")
}

extract_y <- function(stanfit) {
  extract_data(stanfit)$y
}

extract_T <- function(stanfit) {
  extract_data(stanfit)$T
}

extract_K <- function(stanfit) {
  extract_data(stanfit)$K
}

extract_R <- function(stanfit) {
  extract_data(stanfit)$R
}
