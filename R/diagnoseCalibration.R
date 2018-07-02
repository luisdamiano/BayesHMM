diagnose_calibration <- function(spec, N, T = 1000, seed = NULL, ...) {
  if (is.null(seed)) { seed <- .Random.seed[1] }
  dots <- list(...)

  # 1. Draw a sample from the prior predictive density
  mySim      <- do.call(sim, c(list(spec, seed = seed, T = T, chains = 1), dots))
  y          <- extract(mySim, pars = "ypred", permuted = FALSE, inc_warmup = FALSE)[1, 1, ]
  paramTrue  <- extract_obs(mySim, permuted = FALSE)[1, ,]
  paramNames <- names(paramTrue)
  rm(mySim); gc()

  myModel    <- compile(spec)

  # 2. Fit the model to simulated dataset
  l <- vector("list", N * 100 * length(paramNames))
  for (n in 1:N) {
    myFit      <- do.call(
      sampling,
      c(list(spec, stanModel = myModel, y = y, seed = seed + n, switchLabels = TRUE), dots)
    )
    paramFit   <- extract_obs(myFit, permuted = FALSE)
    summaryFit <- monitor(paramFit, print = FALSE)
    timeFit    <- lapply(myFit@sim$samples, attr, "elapsed_time")

    d <- do.call(rbind, get_sampler_params(myFit, inc_warmup = FALSE))

    rm(myFit); gc()

    nIters     <- dim(paramFit)[1]
    nChains    <- dim(paramFit)[2]
    nParams    <- dim(paramFit)[3]

    for (nChain in seq_len(nChains)) {
      for (nParam in seq_len(nParams)) {
        paramName <- paramNames[nParam]
        ind       <- (n - 1) * nChains * nParams + (nChain - 1) * nParams + nParam
        l[[ind]]  <-
          c(
            list(
              model  = spec$name,
              n      = n,
              chain  = nChain,
              param  = paramNames[nParam],
              true   = paramTrue[nParam],
              rank   = sum(paramFit[, nChain, paramName] < paramTrue[paramName]) / nIters
            ),
            summaryFit[paramName, ],
            # summaryFit[paramName, c("se_mean", "sd", "n_eff", "Rhat")],
            list(
              maxTreeDepth = sum(d[, "treedepth__"] == 10),
              divergences  = sum(d[, "divergent__"])
            ),
            timeFit[[nChain]]
          )
      }
    }
  }

  do.call(rbind.data.frame, c(l[!sapply(l, is.null)], stringsAsFactors = FALSE))
}

