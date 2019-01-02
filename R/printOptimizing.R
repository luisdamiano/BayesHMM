#' Prints the result of the model in a human friendly format.
#'
#' @param x An object returned by \code{\link{optimizing}}.
#' @param observation An optional logical indicating whether the observation model should be included in the description. It defaults to TRUE.
#' @param initial An optional logical indicating whether the initial distribution model should be included in the description. It defaults to TRUE.
#' @param transition An optional logical indicating whether the transition model should be included in the description. It defaults to TRUE.
#' @param fixed An optional logical indicating whether the fixed parameters should be included in the description. It defaults to TRUE.
#' @param diagnostics An optional logical indicating whether convergence diagnostics should be included in the description. It defaults to TRUE.
#' @param print An optional logical indicating whether the description should be printing out.
#' @param ... Further arguments passed to \code{\link{cat}}.
#' @return A character string with the model description.
print.Optimization <- function(x, observation = TRUE, initial = TRUE, transition = TRUE, fixed = TRUE, diagnostics = TRUE, print = TRUE, ...) {
  spec           <- extract_spec(x)
  strHeader      <- make_text_header(spec$name)
  strHeader      <- substr(
    make_text_header(spec$name), 1, nchar(make_text_header(spec$name)) - 1
  )

  strObservation <- if (observation) {
    collapse(
      explain_observation(spec),
      print_observation.Optimization(x, print = FALSE),
      make_text_line()
    )
  }

  strInitial     <- if (initial)     {
    collapse(
      explain_initial(spec),
      print_initial.Optimization(x, print = FALSE),
      make_text_line()
    )
  }

  strTransition  <- if (transition)  {
    collapse(
      explain_transition(spec),
      print_transition.Optimization(x, print = FALSE),
      make_text_line()
    )
  }

  strRunning     <- collapse(
    print_running(x, print = FALSE),
    make_text_line()
  )

  strConvergence <- collapse(
    if (x$return_code != 0) {
      sprintf(
        "Return code is %s, and anything that is not zero is problematic. See \nhttps://github.com/stan-dev/stan/blob/develop/src/stan/services/optimize/lbfgs.hpp\n%s",
        x$return_code,
        make_text_line()
      )
    }
  )

  strFooter      <- print_reproducibility(print = FALSE)

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

monitor.Optimization <- function(x, pars, print) {
  fmtFigures <- "%12.3f"
  fmtNames   <- "%12s"
  whitespace <- "\t"
  strNA      <- sprintf(fmtNames, "NA")

  df      <- extract_quantity(x, pars, combine = c)
  titles  <- sprintf(fmtNames, names(df))
  values  <- sprintf(fmtFigures, df)

  sdHess  <- if (is.null(x$hessian)) {
    rep(strNA, length(values))
  } else {
    namesHessian <- string_simplify(colnames(x$hessian))
    namesDF      <- string_simplify(names(df))

    sapply(namesDF, function(s) {
      if (s %in% namesHessian) {
        ind <- which(s == namesHessian)
        sprintf(
          fmtFigures,
          suppressWarnings(sqrt(-1 / diag(x$hessian)[ind]))
        )
      } else {
        strNA
      }
    })
  }

  b       <- 5 # columns per line
  n       <- length(values)
  strOut  <- ""
  for (i in 1:ceiling(n / b)) {
    ind    <- (b*(i - 1)):(min(b*(i), n))
    strOut <- c(
      strOut,
      collapse(
        paste0("Param\t", paste(titles[ind], collapse = whitespace)),
        paste0("MAP  \t", paste(values[ind], collapse = whitespace)),
        paste0("SD   \t", paste(sdHess[ind], collapse = whitespace)),
        "\n"
      )
    )
  }

  if (print)
    cat(strOut)

  strOut
}

print_observation.Optimization <- function(x, print = TRUE) {
  strMon   <- if (is.empty(select_obs_parameters(x))) {
    return("")
  } else {
    monitor.Optimization(x, sort(select_obs_parameters(x)), FALSE)
  }

  strOut   <- collapse(strMon)
  if (print)
    cat(strOut)

  invisible(strOut)
}

print_initial.Optimization <- function(x, diagnostics = TRUE, print = TRUE) {
  strMon   <- if (is.empty(select_initial_parameters(x))) {
    return("")
  } else {
    monitor.Optimization(x, sort(select_initial_parameters(x)), FALSE)
  }

  strOut   <- clean(collapse(strMon))
  if (print)
    cat(strOut)

  invisible(strOut)
}

print_transition.Optimization <- function(x, diagnostics = TRUE, print = TRUE) {
  strMon   <- if (is.empty(select_transition_parameters(x))) {
    return("")
  } else {
    monitor.Optimization(x, sort(select_transition_parameters(x)), FALSE)
  }

  strOut   <- collapse(strMon)
  if (print)
    cat(strOut)

  invisible(strOut)
}
