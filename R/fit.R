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
