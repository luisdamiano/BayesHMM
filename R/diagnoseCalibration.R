diagnose_calibration <- function(spec, N, T = 1000, x = NULL, seed = NULL, cores = NULL, ...) {
  if (is.null(seed)) { seed <- .Random.seed[1] }
  dots <- list(...)

  # 1. Draw a sample from the prior predictive density
  mySim      <- do.call(sim, list(spec, seed = seed, x = x, T = T, iter = max(N + 1, 500), chains = 1))
  ySim       <- extract(mySim, pars = "ypred", permuted = FALSE, inc_warmup = FALSE)
  paramSim   <- extract_obs(mySim, permuted = FALSE)
  paramNames <- dimnames(paramSim)[3][[1]]
  rm(mySim); gc()

  # 2. Fit the model to simulated dataset
  myModel    <- compile(spec)

  if (is.null(cores)) { cores <- parallel::detectCores() }
  cl <- makeCluster(cores, outfile = "")
  registerDoParallel(cl)
  l <- foreach(n = 1:N, .combine = c, .packages = c("BayesHMM", "rstan")) %dopar% {
    y          <- ySim[n, 1, ]
    paramTrue  <- paramSim[n, , ]
    myFit      <- do.call(
      sampling,
      c(list(spec, stanModel = myModel, y = y, x = x, seed = seed + n), dots)
    )

    myFit      <- stan_sort_chain(
      myFit,
      do.call(rbind, lapply(1:T, function(x) { paramTrue } )),
      spec$K
    )

    paramFit   <- extract_obs(myFit, permuted = FALSE)
    summaryFit <- monitor(paramFit, print = FALSE)
    timeFit    <- lapply(myFit@sim$samples, attr, "elapsed_time")

    d <- do.call(rbind, get_sampler_params(myFit, inc_warmup = FALSE))

    rm(myFit); gc()

    nIters     <- dim(paramFit)[1]
    nChains    <- dim(paramFit)[2]
    nParams    <- dim(paramFit)[3]

    l <- vector("list", nChains * nParams)
    for (nChain in seq_len(nChains)) {
      for (nParam in seq_len(nParams)) {
        paramName <- paramNames[nParam]
        ind       <- (nChain - 1) * nParams + nParam
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
            list(
              maxTreeDepth = sum(d[, "treedepth__"] == 10),
              divergences  = sum(d[, "divergent__"])
            ),
            timeFit[[nChain]]
          )
      }
    }

    return(l)
  }
  stopCluster(cl)

  do.call(rbind.data.frame, c(l[!sapply(l, is.null)], stringsAsFactors = FALSE))
}
