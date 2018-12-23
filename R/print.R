#' Prints the result of the model in a human friendly format.
#'
#' @param stanfit An object returned by either \code{\link{fit}} or \code{\link{sampling}}.
#' @param posteriorInterval An optional numeric vector with the quantilies of the posterior marginal distributions.
#' @param spec An object returned by either \code{\link{specify}} or \code{\link{hmm}}.
#' @param observation An optional logical indicating whether the observation model should be included in the description. It defaults to TRUE.
#' @param initial An optional logical indicating whether the initial distribution model should be included in the description. It defaults to TRUE.
#' @param transition An optional logical indicating whether the transition model should be included in the description. It defaults to TRUE.
#' @param fixed An optional logical indicating whether the fixed parameters should be included in the description. It defaults to TRUE.
#' @param print An optional logical indicating whether the description should be printing out.
#' @return A character string with the model description.
#' @export
#' @examples
print_fit <- function(stanfit, posteriorInterval = c(0.025, 0.25, 0.5, 0.75, 0.975),
                      observation = TRUE, initial = TRUE, transition = TRUE, fixed = TRUE, print = TRUE) {
  spec           <- extract_spec(stanfit)
  strHeader      <- make_text_header(spec$name)
  strObservation <- if (observation) {
    cat(explain_observation(spec))
    print_observation(stanfit)
    cat(make_text_line(), "\n")
  }
  strInitial     <- if (initial)     {
    cat(explain_initial(spec))
    print_initial(stanfit)
    cat(make_text_line(), "\n")
  }
  strTransition  <- if (transition)  {
    cat(explain_transition(spec))
    print_transition(stanfit)
    cat(make_text_line(), "\n")
  }
  strFixed       <- if (fixed) {
    "Fixed parameters: not implemented yet."
  }
  strFooter      <- sprintf(
    "Notes for reproducibility: \n%s\n%s\n%s\n%s.\n%s.\n",
    get_time_info(),
    get_session_info(),
    get_package_info(),
    get_rstan_info(),
    get_other_packages_info()
  )

  out <- gsub(
    "\\t",
    get_print_settings()$tab,
    collapse(strHeader, strFixed, strObservation, strInitial, strTransition, strFooter)
  )

  if (print)
    cat(out)

  invisible(out)
  # Seeds, rstan, time, etc, fixed parameters
}

# Internal undocumented print functions -----------------------------------

monitor <- function(stanfit, pars, posteriorInterval) {
  sim <- rstan::extract(stanfit, pars, permuted = FALSE, inc_warmup = FALSE)
  mon <- rstan::monitor(sim, probs = posteriorInterval, print = FALSE)
  colnames(mon) <- c(
    "P Mean", "MCSE", "P SD",
    sprintf("PI%0.1f%%", 100 * posteriorInterval),
    "ESS", "Rhat"
  )
  mon
}

monitor_observation <- function(stanfit, posteriorInterval) {
  monitor(
    stanfit,
    pars = select_obs_parameters(stanfit),
    posteriorInterval = posteriorInterval
  )
}

monitor_initial     <- function(stanfit, posteriorInterval) {
  monitor(
    stanfit,
    pars = select_initial_parameters(stanfit),
    posteriorInterval = posteriorInterval
  )
}

monitor_transition  <- function(stanfit, posteriorInterval) {
  monitor(
    stanfit,
    pars = select_transition_parameters(stanfit),
    posteriorInterval = posteriorInterval
  )
}

print_monitor <- function(mon) {
  mat <- apply(mon, 2, sprintf, fmt = "%8.4f")
  rownames(mat) <- rownames(mon)
  print(as.data.frame(mat))
}

print_observation <- function(stanfit, posteriorInterval = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  strFixed <- format_fixed_parameters(extract_spec(stanfit)$observation$density)
  if (!is.null(strFixed))
    cat(sprintf("Fixed parameters: %s.\n\n", strFixed))

  cat("Summary of posterior samples for the observation model parameters.\n")

  print_monitor(
    monitor_observation(stanfit, posteriorInterval = posteriorInterval)
  )
}

print_initial <- function(stanfit, posteriorInterval = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  cat("Summary of posterior samples for the initial distribution model parameters.\n\n")

  print_monitor(
    monitor_initial(stanfit, posteriorInterval = posteriorInterval)
  )
}

print_transition <- function(stanfit, posteriorInterval = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  cat("Summary of posterior samples for the transition model parameters.\n\n")

  print_monitor(
    monitor_transition(stanfit, posteriorInterval = posteriorInterval)
  )
}

format_fixed_parameters <- function(tree) {
  fixedParam <- densityApply(tree, getFixedParameters)
  format_quantity <- function(x) {
    if (is.matrix(x))
      return(matrix_to_stan(x))

    if (is.vector(x))
      return(vector_to_stan(x))

    paste(x, collapse = "")
  }

  if (!is.null(fixedParam) && length(fixedParam) != 0 && !all(sapply(fixedParam, is.empty)) ) {
    l <- lapply(names(fixedParam), function(paramName) {
      sprintf(
        "%s = %s",
        paramName, format_quantity(fixedParam[[paramName]])
      )
    })

    paste(l, sep = "", collapse = ", ")
  }
}
