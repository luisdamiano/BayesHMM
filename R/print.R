#' Print a description of a Density object in a human friendly format.
#'
#' @param x A \code{\link{Density}} object.
#' @param print An optional logical indicating whether the description should be printed out.
#' @param ... Further arguments passed to \code{\link{cat}}.
#' @return A character string with the density description.
print.Density <- function(x, print = TRUE, ...) {
  strOut <- explain_density(x, print = FALSE)

  if (print)
    cat(strOut)

  invisible(strOut)
}

#' Print a description of a Specification in a human friendly format.
#'
#' @param x A \code{\link{Specification}} object.
#' @param print An optional logical indicating whether the description should be printed out.
#' @param ... Further arguments passed to \code{\link{cat}}.
#' @return A character string with the density description.
print.Specification <- function(x, print = TRUE, ...) {
  strOut <- explain(x, print = FALSE)

  if (print)
    cat(strOut)

  invisible(strOut)
}

#' Print the result of the model in a human friendly format.
#'
#' @param x An object returned by either \code{\link{fit}} or \code{\link{draw_samples}}.
#' @param posteriorInterval An optional numeric vector with the quantilies of the posterior marginal distributions.
#' @param observation An optional logical indicating whether the observation model should be included in the description. It defaults to TRUE.
#' @param initial An optional logical indicating whether the initial distribution model should be included in the description. It defaults to TRUE.
#' @param transition An optional logical indicating whether the transition model should be included in the description. It defaults to TRUE.
#' @param fixed An optional logical indicating whether the fixed parameters should be included in the description. It defaults to TRUE.
#' @param diagnostics An optional logical indicating whether convergence diagnostics should be included in the description. It defaults to TRUE.
#' @param print An optional logical indicating whether the description should be printed out.
#' @param ... Further arguments passed to \code{\link{cat}}.
#' @return A character string with the model description.
print.stanfit <- function(x, posteriorInterval = c(0.025, 0.25, 0.5, 0.75, 0.975),
                      observation = TRUE, initial = TRUE, transition = TRUE, fixed = TRUE, diagnostics = TRUE, print = TRUE, ...) {
  spec           <- extract_spec(x)

  strHeader      <- make_text_header(spec$name)
  strHeader      <- substr(strHeader, 1, nchar(strHeader) - 1)

  strObservation <- if (observation) {
    collapse(
      explain_observation(spec),
      print_observation(x, print = FALSE),
      make_text_line()
    )
  }

  strInitial     <- if (initial)     {
    collapse(
      explain_initial(spec),
      print_initial(x, print = FALSE),
      make_text_line()
    )
  }

  strTransition  <- if (transition)  {
    collapse(
      explain_transition(spec),
      print_transition(x, print = FALSE),
      make_text_line()
    )
  }

  strRunning     <- collapse(
    print_running(x, print = FALSE),
    make_text_line()
  )

  strConvergence <- if (diagnostics & !is.null(extract_y(x))) {
    collapse(
      print_convergence(x, print = FALSE),
      make_text_line()
    )
  } else {
    ""
  }

  strFooter      <- collapse(
    print_reproducibility(print = FALSE)
  )

  strOut         <- gsub(
    "\\t",
    get_print_settings()$tab,
    collapse(strHeader, strObservation, strInitial, strTransition, strRunning, strConvergence, strFooter)
  )

  strOut         <- clean(strOut)

  if (print)
    cat(strOut, ...)

  invisible(strOut)
}

# Overwrite stanfit's show method
# I know this may be controversial but sorry rstan =/
setMethod("show", "stanfit",
          function(object) {
            print.stanfit(x = object)
          })

# Internal undocumented print functions -----------------------------------

print_reproducibility <- function(print) {
  strOut <- sprintf(
    "Notes for reproducibility: \n - %s\n - %s\n - %s\n - %s\n - %s\n - %s\n",
    get_time_info(),
    get_platform_info(),
    get_R_info(),
    get_package_info(),
    get_rstan_info(),
    get_other_packages_info()
  )

  if (print)
    cat(strOut)

  invisible(strOut)
}

print_running <- function(fit, print) {
  strOut <- sprintf(
    "Model ran on %s at %s using seed %s.\n%s\nTime elapsed in minutes: \n%s.",
    format(extract_date(fit), "%Y-%m-%d"),
    format(extract_date(fit), "%T"),
    extract_seed(fit),
    if (is.stanfit(fit)) {
      wrap(
        sprintf(
          "Sample size: %s kept iterations (%s total, %s warmup, thin every %s).",
          sum(extract_sample_size(fit)),
          sum(extract_n_iterations(fit)),
          sum(extract_n_warmup(fit)),
          paste(unique(extract_n_thin(fit)), sep = ", ")
        )
      )
    },
    print_running_time(fit, print = FALSE)
  )

  if (print)
    cat(strOut)

  invisible(strOut)
}

print_running_time <- function(fit, print) {
  x         <- extract_time(fit)

  strNames  <- if (!is.null(rownames(x))) {
    gsub(":", " ", toproper(rownames(x)))
  } else {
    sprintf("Optimization run %s", seq_along(nrow(x)))
  }

  strValues <- apply(x, 1, function(r) {
    paste(
      paste(names(r), sprintf("%0.3f", r)),
      collapse = ", "
    )
  })

  strOut    <- collapse(
    wrap(sprintf(" - %s: %s", strNames, strValues))
  )

  if (print)
    cat(strOut)

  invisible(strOut)
}

print_convergence <- function(stanfit, print) {
  diags <- extract_diagnostics(stanfit, pars = select_all_parameters(stanfit))

  diag_chain <- function(diags, colName, diagMessage) {
    ind <- which(diags$chains[[colName]] > 0)
    if (is.empty(ind))
      return("")

    collapse(
      sprintf(
        "Chain %s: %s %s.",
        diags$chains$chain[ind],
        diags$chains[[colName]][ind],
        diagMessage
      )
    )
  }

  diag_param <- function(diags, ind, value, diagMessage) {
    if (!any(ind))
      return("")

    collapse(
      sprintf(
        "Parameter %s: %s (%-0.2f).",
        diags$parameters$parameter[ind],
        diagMessage,
        value
      )
    )
  }

  mon        <- monitor(
    stanfit,
    select_all_parameters(stanfit),
    posteriorInterval = 0.5,
    diagnostics = FALSE
  )

  sampleSize <- extract_sample_size(stanfit)
  indESS     <- diagnose_ess(mon[, "ESS"], sampleSize)
  indMCSE    <- diagnose_mcse(mon[, "MCSE"], mon[, "P SD"])
  indRhat    <- diagnose_rhat(mon[, "Rhat"])

  strDiver   <- diag_chain(diags, "divergences" , "divergent iterations")
  strDepth   <- diag_chain(diags, "maxTreeDepth", "iterations hitting max tree depth")
  strLeaps   <- diag_chain(diags, "maxNLeapfrog", "iterations hitting max number of leapfrogs")
  strESS     <- diag_param(
    diags,
    indESS,
    (mon[, "ESS"] / sampleSize)[indESS],
    "effective sample size < 10% of kept iterations"
  )
  strMCSE    <- diag_param(
    diags,
    indMCSE,
    (mon[, "MCSE"] / mon[, "P SD"])[indMCSE],
    "Monte Carlo error > 15% of posterior std dev"
  )
  strRhat    <- diag_param(
    diags,
    indRhat,
    mon[, "Rhat"][indRhat],
    "potential scale reduction factor > 1.1"
  )

  strOut     <- collapse(
    "Convergence diagnostics",
    strDiver, strDepth, strLeaps, strESS, strMCSE, strRhat
  )

    # diag_chain(diags, "divergences" , "divergent iterations"),
    # diag_chain(diags, "maxTreeDepth", "iterations hitting max tree depth"),
    # diag_chain(diags, "maxNLeapfrog", "iterations hitting max number of leapfrogs"),
    # diag_param(
    #   diags,
    #   indESS,
    #   (mon[, "ESS"] / sampleSize)[indESS],
    #   "effective sample size < 10% of kept iterations"
    # ),
    # diag_param(
    #   diags,
    #   indMCSE,
    #   (mon[, "MCSE"] / mon[, "P SD"])[indMCSE],
    #   "Monte Carlo error > 15% of posterior standard deviation"
    # ),
    # diag_param(
    #   diags,
    #   indRhat,
    #   mon[, "Rhat"][indRhat],
    #   "potential scale reduction factor > 1.1"
    # )
  # )

  if (print)
    cat(strOut)

  invisible(strOut)
}

monitor <- function(stanfit, pars, posteriorInterval, diagnostics) {
  sim <- rstan::extract(stanfit, pars, permuted = FALSE, inc_warmup = FALSE)
  mon <- rstan::monitor(sim, probs = posteriorInterval, print = FALSE)
  rownames(mon) <- sprintf("%-7s", rownames(mon))
  colnames(mon) <- c(
    "P Mean", "MCSE", "P SD",
    sprintf("PI%0.1f", 100 * posteriorInterval),
    "ESS", "Rhat"
  )

  if (diagnostics) {
    ind    <- diagnose_parameters(stanfit, pars)
    preStr <- ifelse(ind, "! ", "  ")
    rownames(mon) <- paste0(preStr, rownames(mon))
  }

  mon
}

monitor_observation <- function(stanfit, posteriorInterval, diagnostics) {
  monitor(
    stanfit           = stanfit,
    pars              = select_obs_parameters(stanfit),
    posteriorInterval = posteriorInterval,
    diagnostics       = diagnostics
  )
}

monitor_initial     <- function(stanfit, posteriorInterval, diagnostics) {
  monitor(
    stanfit           = stanfit,
    pars              = select_initial_parameters(stanfit),
    posteriorInterval = posteriorInterval,
    diagnostics       = diagnostics
  )
}

monitor_transition  <- function(stanfit, posteriorInterval, diagnostics) {
  monitor(
    stanfit           = stanfit,
    pars              = select_transition_parameters(stanfit),
    posteriorInterval = posteriorInterval,
    diagnostics       = diagnostics
  )
}

print_monitor <- function(mon, print) {
  fmt           <- get_print_settings()$figures
  mat           <- apply(mon, 2, sprintf, fmt = fmt)
  rownames(mat) <- rownames(mon)
  out           <- collapse(
    capture.output(format(as.data.frame(mat)))
  )

  if (print)
    print(out)

  invisible(out)
}

print_observation <- function(stanfit,
                              posteriorInterval = c(0.025, 0.25, 0.5, 0.75, 0.975),
                              diagnostics = TRUE, print = TRUE) {
  # strFixed <- format_fixed_parameters(extract_spec(stanfit)$observation$density)
  # strFixed <- if (!is.null(strFixed)) {
  #   sprintf("Fixed parameters: %s.\n", strFixed)
  # } else {
  #   ""
  # }

  # strTag   <-
  #   "Summary of posterior samples for the observation model parameters.\n"

  strMon   <- if (is.empty(select_obs_parameters(stanfit))) {
    return("")
  } else {
    print_monitor(
      mon   = monitor_observation(stanfit, posteriorInterval = posteriorInterval, diagnostics = diagnostics),
      print = FALSE
    )
  }

  strOut   <- collapse(strMon)
  if (print)
    cat(strOut)

  invisible(strOut)
}

print_initial <- function(stanfit,
                          posteriorInterval = c(0.025, 0.25, 0.5, 0.75, 0.975),
                          diagnostics = TRUE, print = TRUE) {
  # strFixed <- format_fixed_parameters(extract_spec(stanfit)$initial$density)
  # strFixed <- if (!is.null(strFixed)) {
  #   sprintf("Fixed parameters: %s.\n", strFixed)
  # } else {
  #   ""
  # }

  # strTag   <-
  #   "Summary of posterior samples for the initial distribution model parameters.\n"

  strMon   <- if (is.empty(select_initial_parameters(stanfit))) {
    return("")
  } else {
    print_monitor(
      mon   = monitor_initial(stanfit, posteriorInterval = posteriorInterval, diagnostics = diagnostics),
      print = FALSE
    )
  }

  strOut   <- collapse(strMon)
  if (print)
    cat(strOut)

  invisible(strOut)
}

print_transition <- function(stanfit,
                             posteriorInterval = c(0.025, 0.25, 0.5, 0.75, 0.975),
                             diagnostics = TRUE, print = TRUE) {
  # strFixed <- format_fixed_parameters(extract_spec(stanfit)$transition$density)
  # strFixed <- if (!is.null(strFixed)) {
  #   sprintf("Fixed parameters: %s.\n", strFixed)
  # } else {
  #   ""
  # }

  # strTag   <-
  #   "Summary of posterior samples for the transition model parameters.\n"

  strMon   <- if (is.empty(select_transition_parameters(stanfit))) {
    return("")
  } else {
    print_monitor(
      mon   = monitor_transition(stanfit, posteriorInterval = posteriorInterval, diagnostics = diagnostics),
      print = FALSE
    )
  }

  strOut   <- collapse(strMon)
  if (print)
    cat(strOut)

  invisible(strOut)
}

format_fixed_parameters <- function(tree) {
  fixedParam <- densityApply(tree, getFixedParameters)

  if (!is.null(fixedParam) && length(fixedParam) != 0 && !all(sapply(fixedParam, is.empty)) ) {
    l <- lapply(names(fixedParam), function(paramName) {
      sprintf(
        "%s = %s",
        paramName, numeric_to_stan(fixedParam[[paramName]])
      )
    })

    paste(l, sep = "", collapse = ", ")
  }
}
