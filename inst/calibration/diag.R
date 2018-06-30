permutations <- function(n, r, v = 1:n) {
  # Simplified implementation from gtools::permutations
  # Credits for gtools::permutations
  if (r == 1)
    matrix(v, n, 1)
  else if (n == 1)
    matrix(v, 1, r)
  else {
    X <- NULL
    for (i in 1:n) {
      X <- rbind(X, cbind(v[i], Recall(n - 1, r - 1, v[-i])))
    }
    X
  }
}

reorder_within <- function(x, y, K) {
  # x is the Chain reference
  # paraNames <- unique(gsub(pattern = "[0-9]", "", colnames(x)))
  P     <- permutations(K, K)
  xMed  <- apply(x, 2, median)
  sqSum <- apply(P, 1, function(rOrder) {
    yNewMed <- apply(y[, rOrder], 2, median)
    sum((xMed - yNewMed)^2)
  })
  y[, P[which.min(sqSum), ]]
}

reorder_between <- function(x, y, K) {
  nChains <- dim(y)[2]

  for (nChain in seq_len(nChains)) {
    y[, nChain, ] <- reorder_within(
      x,
      y[, nChain, ],
      K
    )
  }

  y
}

diagnose_calibration <- function(spec, N, T = 1000, seed = NULL, ...) {
  if (is.null(seed)) { seed <- .Random.seed[1] }
  dots <- list(...)

  # 1. Draw a sample from the prior predictive density
  mySim      <- do.call(sim, c(list(spec, seed = seed, T = T, chains = 1), dots))
  y          <- extract(mySim, pars = "ypred", permuted = FALSE)[1, 1, ]
  paramTrue  <- sapply(
    lapply(extract_obs(mySim), "[", 1),
    function(x) { rep(x, length(y) )}
  )
  paramNames <- colnames(paramTrue)
  rm(mySim); gc()

  # 2. Fit the model to simulated dataset
  l <- vector("list", N * 100 * length(paramNames))
  for (n in 1:N) {
    myFit      <- do.call(fit, c(list(spec, y, seed = seed + n), dots))
    paramFit   <- extract_obs(myFit, permuted = FALSE)
    paramFit   <- reorder_between(paramTrue, paramFit, spec$K)
    summaryFit <- summary_obs(myFit)$summary
    rm(myFit); gc()

    nIters     <- dim(paramFit)[1]
    nChains    <- dim(paramFit)[2]
    nParams    <- dim(paramFit)[3]

    for (nChain in seq_len(nChains)) {
      for (nParam in seq_len(nParams)) {
        paramName <- paramNames[nParam]
        ind       <- (n - 1) * N + (nChain - 1) * nChains + nParam
        l[[ind]]  <-
          c(
            list(
              model  = spec$name,
              n      = n,
              chain  = nChain,
              param  = paramNames[nParam],
              rank   = sum(paramFit[, nChain, paramName] < paramTrue[, paramName]) / nIters
            ),
            summaryFit[paramName, c("se_mean", "sd", "n_eff", "Rhat")]
          )
      }
    }
  }

  do.call(rbind.data.frame, c(l[!sapply(l, is.null)], stringsAsFactors = FALSE))
}
