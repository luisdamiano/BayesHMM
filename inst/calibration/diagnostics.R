convergence_diagnostics <- function(stanfit) {
  pars <- select_obs_parameters(stanfit)
  s <- summary(stanfit, pars = pars)$summary
  e <- extract(stanfit, pars = pars)
  d <- do.call(rbind, get_sampler_params(stanfit, inc_warmup = FALSE))
  N <- NROW(d)

  parTrue

  sapply(1:length(parTrue), function(i) {
    sum(e[[i]] < parTrue[i]) / N
  })

  # TRUE is good.
  checkRhat <- apply(s, 1, function(r) {
    r["Rhat"] < 1.1
  })

  checkTrue(
    all(checkRhat),
    sprintf(
      "Following parameters have RHat >= 1.1: %s",
      paste0(pars[!checkRhat], collapse = ", ")
    )
  )

  checkESS  <- apply(s, 1, function(r) {
    r["n_eff"] / N > 0.1
  })

  checkTrue(
    all(checkESS),
    sprintf(
      "Following parameters have ESS / N <= 0.1: %s",
      paste0(pars[!checkESS])
    )
  )

  checkMCSE <- apply(s, 1, function(r) {
    r["se_mean"] / r["sd"] < 0.2 #0.1
  })

  checkTrue(
    all(checkMCSE),
    sprintf(
      "Following parameters have MCSE / SD >= 0.1: %s",
      paste0(pars[!checkMCSE])
    )
  )

  checkDiv  <- sum(d[, "divergent__"])

  checkTrue(
    checkDiv == 0,
    sprintf("Divergences detected: %d (%0.2f%%).", checkDiv, 100 * checkDiv / N)
  )

  checkTree <- sum(d[, "treedepth__"] == 10)

  checkTrue(
    checkTree == 0,
    sprintf("Max tree depth: %d (%0.2f%%).", checkTree, 100 * checkTree / N)
  )
}

# convergence_diagnostics <- function(stanfit) {
#   pars <- select_obs_parameters(stanfit)
#   s <- summary(stanfit, pars = pars)$summary
#   d <- do.call(rbind, get_sampler_params(stanfit, inc_warmup = FALSE))
#   N <- NROW(d)
#
#   # TRUE is good.
#   checkRhat <- apply(s, 1, function(r) {
#     r["Rhat"] < 1.1
#   })
#
#   checkTrue(
#     all(checkRhat),
#     sprintf(
#       "Following parameters have RHat >= 1.1: %s",
#       paste0(pars[!checkRhat], collapse = ", ")
#     )
#   )
#
#   checkESS  <- apply(s, 1, function(r) {
#     r["n_eff"] / N > 0.1
#   })
#
#   checkTrue(
#     all(checkESS),
#     sprintf(
#       "Following parameters have ESS / N <= 0.1: %s",
#       paste0(pars[!checkESS])
#     )
#   )
#
#   checkMCSE <- apply(s, 1, function(r) {
#     r["se_mean"] / r["sd"] < 0.2 #0.1
#   })
#
#   checkTrue(
#     all(checkMCSE),
#     sprintf(
#       "Following parameters have MCSE / SD >= 0.1: %s",
#       paste0(pars[!checkMCSE])
#     )
#   )
#
#   checkDiv  <- sum(d[, "divergent__"])
#
#   checkTrue(
#     checkDiv == 0,
#     sprintf("Divergences detected: %d (%0.2f%%).", checkDiv, 100 * checkDiv / N)
#   )
#
#   checkTree <- sum(d[, "treedepth__"] == 10)
#
#   checkTrue(
#     checkTree == 0,
#     sprintf("Max tree depth: %d (%0.2f%%).", checkTree, 100 * checkTree / N)
#   )
# }
#
# # true: vector of size P (P = number of parameters)
# # estimateInterval: matrix 2 rows (lower and upper bounds), P columns
# check_coverage <- function(true, estimateInterval) {
#   if (length(true) != NCOL(estimateInterval)) {
#     stop("The test is misspecified")
#   }
#
#   true <- sort(true)
#   estimateInterval  <- estimateInterval[, order(estimateInterval[1, ])]
#
#   sapply(1:length(true), function(i) {
#     estimateInterval[1, i] < true[i] && true[i] < estimateInterval[2, i]
#   })
# }
#
# # true: vector of size P (P = number of parameters)
# # estimateInterval: matrix 2 rows (lower and upper bounds), P columns
# coverage_diagnostics <- function(true, estimateInterval) {
#   if (length(true) != NCOL(estimateInterval)) {
#     stop("The test is misspecified")
#   }
#
#   true <- sort(true)
#   estimateInterval  <- estimateInterval[, order(estimateInterval[1, ])]
#
#   checkTrue(
#     all(
#       sapply(1:length(true), function(i) {
#         estimateInterval[1, i] < true[i] && true[i] < estimateInterval[2, i]
#       })
#     ),
#     "The 80% posterior interval does not include the true parameter."
#   )
# }
