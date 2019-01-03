# extract_n_chains ------------------------------------------------------------
#' Extract the number of chains \emph{M}.
#'
#' @param stanfit An object returned by either \code{\link{fit}} or \code{\link{draw_samples}}.
#' @return The number of chains \emph{M} used to fit the model.
extract_n_chains <- function(stanfit) {
  UseMethod("extract_n_chains", stanfit)
}

#' @keywords internal
#' @inherit extract_n_chains
extract_n_chains.stanfit <- function(stanfit) {
  stanfit@sim$chains
}

# extract_n_iterations ---------------------------------------------------------
#' Extract the number of total iterations.
#'
#' @param stanfit An object returned by either \code{\link{fit}} or \code{\link{draw_samples}}.
#' @return The number of total iterations.
extract_n_iterations <- function(stanfit) {
  UseMethod("extract_n_iterations", stanfit)
}

#' @keywords internal
#' @inherit extract_n_iterations
extract_n_iterations.stanfit <- function(stanfit) {
  sum(sapply(stanfit@stan_args, `[[`, "iter"))
}

# extract_n_thin ---------------------------------------------------------
#' Extract the thinning periodicity.
#'
#' @param stanfit An object returned by either \code{\link{fit}} or \code{\link{draw_samples}}.
#' @return The the thinning periodicity.
extract_n_thin <- function(stanfit) {
  UseMethod("extract_n_thin", stanfit)
}

#' @keywords internal
#' @inherit extract_n_thin
extract_n_thin.stanfit <- function(stanfit) {
  sapply(stanfit@stan_args, `[[`, "thin")
}

# extract_n_warmup ---------------------------------------------------------
#' Extract the number of warmup iterations.
#'
#' @param stanfit An object returned by either \code{\link{fit}} or \code{\link{draw_samples}}.
#' @return The number of warmup iterations.
extract_n_warmup <- function(stanfit) {
  UseMethod("extract_n_warmup", stanfit)
}

#' @keywords internal
#' @inherit extract_n_warmup
extract_n_warmup.stanfit <- function(stanfit) {
  sum(sapply(stanfit@stan_args, `[[`, "warmup"))
}

# extract_sample_size ---------------------------------------------------------
#' Extract the number of iterations kept after warmup.
#'
#' @param stanfit An object returned by either \code{\link{fit}} or \code{\link{draw_samples}}.
#' @return The number of iterations kept after warmup.
extract_sample_size <- function(stanfit) {
  UseMethod("extract_sample_size", stanfit)
}

#' @keywords internal
#' @inherit extract_sample_size
extract_sample_size.stanfit <- function(stanfit) {
  sum(stanfit@sim$n_save - stanfit@sim$warmup2)
}

#' Check if it is an object created by \code{\link{draw_samples}}.
#'
#' @keywords internal
#' @param x An object.
#' @return TRUE if it is an object created by \code{\link{draw_samples}}.
is.stanfit <- function(x) {
  inherits(x, "stanfit")
}

# extract_diagnostics ---------------------------------------------------------
#' Extract MCMC convergence and posterior predictive diagnostics.
#'
#' @param stanfit An object returned by either \code{\link{fit}} or \code{\link{draw_samples}}.
#' @param pars A vector of characters with the name of the quantities to be extracted. The characters strings may include regular expressions. Further, wildcards are automatically translated into regex: \emph{?} matches a single character, while \emph{*} matches any character string including an empty one. For example, \emph{?pred} will match both ypred and zpred, and \emph{z*} will match zstar and zpred. It defaults to all the observation model parameters.
#' @param trueParameters An optional numeric vector with the true value of the parameter vector.
#' @return A named list with two elements. The first element \emph{chains} is a data.frame with Markov-chain Monte Carlo convergence diagnostics (number of divergences, number of times max tree depth is reached, maximum leapfrogs, warm up and sampling times) and posterior predictive checks (observation ranks, Kolmogorov-Smirnov statistic for observed sample vs posterior predictive samples). The second element, \emph{parameters}, compare true versus estimated values for the unknown quantities (mean, sd, quantiles and other posterior measures, Monte Carlo standard error, estimated sample size, R Hat, and rank).
extract_diagnostics <- function(stanfit, pars = select_obs_parameters(stanfit),
                                trueParameters = NULL) {
  UseMethod("extract_diagnostics", stanfit)
}

#' @keywords internal
#' @inherit extract_diagnostics
extract_diagnostics.stanfit <- function(stanfit,
                                        pars = select_obs_parameters(stanfit),
                                        trueParameters = NULL) {
  d       <- get_diagnose_parameters(stanfit, trueParameters, pars)
  nChains <- extract_n_chains(stanfit)
  spec    <- extract_spec(stanfit)

  list(
    chains = data.frame(
      model = spec$name,
      chain = 1:nChains,
      get_diagnose_chain_convergence(stanfit),
      get_diagnose_ppredictive(stanfit),
      row.names = NULL,
      stringsAsFactors = FALSE
    ),
    parameters = data.frame(
      model = spec$name,
      chain     = rep(1:nChains, each = length(unique(rownames(d)))),
      parameter = rownames(d),
      d,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  )
}
