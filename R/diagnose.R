diagnose_ess <- function(ess, ss) {
  ess / ss < 0.1
}

diagnose_mcse <- function(mcse, psd) {
  mcse / psd > 0.15
}

diagnose_rhat <- function(rhat) {
  rhat > 1.1
}

diagnose_parameters <- function(stanfit, pars) {
  pars       <- match_parameter_names(stanfit, pars)
  sim        <- rstan::extract(stanfit, pars, permuted = FALSE, inc_warmup = FALSE)
  mon        <- rstan::monitor(sim, probs = 0.5, print = FALSE)
  # mon        <- monitor(stanfit, pars = c("mu11", "mu21"), 0.5)
  sampleSize <- extract_sample_size(stanfit)
  ind        <-
    diagnose_rhat(mon[, "Rhat"]) |
    diagnose_ess(mon[, "n_eff"], sampleSize) |
    diagnose_mcse(mon[, "se_mean"], mon[, "sd"])

  ind
}
