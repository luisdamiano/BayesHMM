# loo <- function(stanfit, mergeChains = FALSE, ...) {
#   nChains <- extract_n_chains(stanfit)
#   logLike <-
#     if (mergeChains) {
#       warning("mergeChains = TRUE not yet implemented.")
#       apply(extract(stanfit, pars = "logalpha")[[1]], c(1, 3), sum)
#     } else {
#       apply(extract(stanfit, pars = "logalpha")[[1]], c(1, 3), sum)
#     }
#   relEff  <- loo::relative_eff(
#     x        = exp(logLike),
#     chain_id = rep(seq_len(nChains), each = nrow(logLike) / nChains)
#   )
#
#   loo::loo(logLike, r_eff = relEff, ...)
# }
#
