
# extract_quantity --------------------------------------------------------
extract_quantity <- function(fit, ...) { UseMethod("extract_quantity", fit) }

# Should DROP as much as possible
extract_quantity.Optimization <- function(fit, pars = "", chain = 1, reduce = NULL, combine = NULL, ...) {
  reduce <- NULL # Can't reduce in-chain cause there are not iterations
  ind <- do.call(c, lapply(pars, function(par) {
    grep(
      pattern = glob2rx(sprintf("%s*", par)),
      x       = names(fit$par)
    )})
  )

  xs  <- fit$par[ind]

  l   <- sapply(xs, function(x) {
    drop(if (is.null(reduce)) { x } else { reduce(x) })
  }, simplify = FALSE, USE.NAMES = TRUE)

  if (is.null(combine)) { l } else { drop(do.call(combine, l)) }
}

extract_quantity.stanfit <- function(fit, pars, reduce = NULL, combine = NULL, chain = "all", ...) {
  ext <- function(p) {
    # extract and set dimensions: x[iteration, chain, parDims ...]
    x           <- rstan::extract(fit, pars = p, permuted = FALSE, ...)
    parDims     <- if (is.empty(fit@par_dims[[p]])) { 1 } else { fit@par_dims[[p]] }
    dim(x)      <- c(dim(x)[1:2], parDims)

    # reduce but keep dimensions
    if (!is.null(reduce)) {
      oDim   <- dim(x)
      x      <- apply(x, seq.int(2, length(oDim)), reduce)
      dim(x) <- replace(oDim, 1, length(x) / prod(oDim[-1]))
    }

    # subset/merge but keep dimensions
    if (chain == "all")
      invisible({})

    if (chain %in% seq_len(extract_n_chains(fit))) {
      oDim   <- dim(x)
      x      <- x[slice.index(x, 2) == chain]
      dim(x) <- replace(oDim, 2, 1)
    }

    if (chain == "merge")
      warning("extract_quantity.stanfit: Merging chains not yet implemented.")

    return(drop(x))
  }

  l <- sapply(pars, ext, simplify = FALSE, USE.NAMES = TRUE)

  if (is.null(combine))
    return(l)

  drop(do.call(combine, l))
}

setMethod("extract_quantity", "stanfit", extract_quantity.stanfit)

# extract_parameter_names -------------------------------------------------
extract_parameter_names <- function(fit, ...) { UseMethod("extract_parameter_names", fit) }

extract_parameter_names.Optimization <- function(fit) {
  names(fit$par)
}

extract_parameter_names.stanfit <- function(fit) {
  fit@model_pars
}

setMethod("extract_parameter_names", "stanfit", extract_parameter_names.stanfit)

# extract_time ------------------------------------------------------------
extract_time     <- function(fit, ...) { UseMethod("extract_time", fit) }

extract_time.Optimization <- function(fit) {
  attr(fit, "systemTime")
}

extract_time.stanfit <- function(fit) {
  rstan::get_elapsed_time(fit)
}

setMethod("extract_time", "stanfit", extract_time.stanfit)

# classify_alpha ----------------------------------------------------------
classify_alpha   <- function(fit, ...) { UseMethod("classify_alpha", fit) }

classify_alpha.Optimization <- function(fit, reduce = NULL, chain = NULL) {
  alpha  <- extract_alpha(fit)[[1]]
  apply(alpha, 2, which.max)
}

classify_alpha.stanfit <- function(fit, reduce = median, chain = 1) {
  alpha  <- extract_alpha(fit, reduce = reduce, chain = chain)[[1]]
  apply(alpha, 2, which.max)
}

setMethod("classify_alpha", "stanfit", classify_alpha.stanfit)

# classify_gamma ----------------------------------------------------------
classify_gamma   <- function(fit, ...) { UseMethod("classify_gamma", fit) }

classify_gamma.Optimization <- function(fit, reduce = NULL, chain = NULL) {
  gamma  <- extract_gamma(fit)[[1]]
  apply(gamma, 2, which.max)
}

classify_gamma.stanfit <- function(fit, reduce = median, chain = 1) {
  if (chain == "all") { warning("chain = all not gonna work") }
  gamma  <- extract_gamma(fit, reduce = reduce, chain = chain)[[1]]
  apply(gamma, 2, which.max)
}

setMethod("classify_gamma", "stanfit", classify_gamma.stanfit)

# classify_zstar ----------------------------------------------------------
classify_zstar   <- function(fit, ...) { UseMethod("classify_zstar", fit) }

classify_zstar.Optimization <- function(fit, reduce = NULL, chain = NULL) {
  extract_zstar(fit)[[1]]
}

classify_zstar.stanfit <- function(fit, reduce = posterior_mode, chain = 1) {
  extract_zstar(fit, reduce = reduce, chain = chain)[[1]]
}

setMethod("classify_zstar", "stanfit", classify_zstar.stanfit)
