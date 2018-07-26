# stuff that is common for both stanfit and stanoptim
select_parameters <- function(fit, observation = TRUE,
                              initial = TRUE, transition = TRUE) {
  spec <- extract_spec(fit)
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
  fitNames      <- extract_parameter_names(fit)
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

select_initial_parameters <- function(fit) {
  select_parameters(fit, FALSE, TRUE, FALSE)
}

select_transition_parameters <- function(fit) {
  select_parameters(fit, FALSE, FALSE, TRUE)
}

select_all_parameters <- function(fit) {
  select_parameters(fit, TRUE, TRUE, TRUE)
}

extract_parameters <- function(fit, observation = TRUE,
                           initial = TRUE, transition = TRUE, ...) {
  pars <- select_parameters(fit, observation, initial, transition)
  sapply(pars, function(p) { extract_quantity(fit, p, ...) })
}

extract_obs_parameters <- function(fit, ...) {
  extract_quantity(fit, pars = select_obs_parameters(fit), ...)
}

extract_alpha <- function(fit, ...) {
  extract_quantity(fit, "alpha", ...)[[1]]
}

extract_gamma <- function(fit, ...) {
  extract_quantity(fit, "gamma", ...)[[1]]
}

extract_zstar <- function(fit, ...) {
  extract_quantity(fit, "zstar", ...)[[1]]
}

extract_ypred <- function(fit, ...) {
  extract_quantity(fit, "ypred", ...)[[1]]
}

extract_ysim  <- function(fit, n = NULL, ...) {
  ysim <- extract_quantity(fit, "ypred", ...)[[1]]

  if (is.null(n))
    return(ysim)

  out      <- ysim[slice.index(ysim, 1) == n]
  dim(out) <- dim(ysim)[-1]
  out
}

extract_zpred <- function(fit, ...) {
  extract_quantity(fit, "zpred", ...)[[1]]
}

extract_data <- function(fit) {
  attr(fit, "data")
}

extract_y <- function(fit) {
  extract_data(fit)$y
}

extract_T <- function(fit) {
  extract_data(fit)$T
}

extract_K <- function(fit) {
  extract_data(fit)$K
}

extract_R <- function(fit) {
  extract_data(fit)$R
}

extract_spec <- function(fit) {
  attr(fit, "spec")
}

extract_filename <- function(fit) {
  attr(fit, "filename")
}

plot_obs <- function(fit, ...) {
  plot(fit, pars = select_obs_parameters(fit), ...)
}

print_obs <- function(fit, ...) {
  print(fit, pars = select_obs_parameters(fit), ...)
}

print_all <- function(fit, ...) {
  print(fit, pars = select_all_parameters(fit), ...)
}

browse_model <- function(fit) {
  browseURL(extract_filename(fit))
}
