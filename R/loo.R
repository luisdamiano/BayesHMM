loo <- function(stanfit, mergeChains = FALSE, ...) {
  logLike <-
    if (mergeChains) {
      apply(extract(stanfit, pars = "logalpha")[[1]], c(1, 3), sum)
    } else {
      warning("mergeChains = TRUE not yet implemented.")
      apply(extract(stanfit, pars = "logalpha")[[1]], c(1, 3), sum)
    }
  relEff  <- loo::relative_eff(exp(logLike))

  loo(logLike, r_eff = relEff, ...)
}
