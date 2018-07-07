diagnose_calibration2 <- function(spec, N, T = 1000, x = NULL, seed = NULL, cores = NULL, ...) {
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

diagnose_calibration <- function(spec, N, T = 1000, x = NULL, seed = 9000, cores = NULL, ...) {
  dots <- list(...)

  # 1. Draw a sample from the prior predictive density
  mySim      <- do.call(sim, list(spec, seed = seed, x = x, T = T, iter = max(N + 1, 500), chains = 1))
  ySim       <- extract(mySim, pars = "ypred", permuted = FALSE, inc_warmup = FALSE)
  paramSim   <- extract_obs(mySim, permuted = FALSE)
  rm(mySim); gc()

  # 2. Fit the model to simulated dataset
  myModel    <- compile(spec)

  if (is.null(cores)) { cores <- parallel::detectCores() / 2 }
  cl <- makeCluster(cores, outfile = "")
  registerDoParallel(cl)
  l <- foreach(n = 1:N, .combine = c, .packages = c("BayesHMM", "rstan")) %dopar% {
    y          <- ySim[n, 1, ] # Chain 1
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

    myDiag     <- diagnose(myFit, trueParameters = paramTrue)
    myDiag$chains$seed  <- seed
    myDiag$chains$n     <- n
    myDiag$parameters$n <- n
    rm(myFit); gc()
    return(myDiag)
  }
  stopCluster(cl)

  sapply(unique(names(l)), function(name) {
    out <- do.call(rbind.data.frame, l[which(names(df) == name)])
    rownames(out) <- NULL
    out
  }, simplify = FALSE)
}

diagnose <- function(stanfit, pars = select_obs_parameters(stanfit), trueParameters = NULL) {
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

get_diagnose_chain_convergence <- function(stanfit) {
  fun <- function(nChain, stanfit) {
    d <- get_sampler_params(stanfit, inc_warmup = FALSE)[[nChain]]

    c(
      divergences  = sum(d[, "divergent__"]),
      maxTreeDepth = sum(d[, "treedepth__"] == 10),
      maxNLeapfrog = max(d[, "n_leapfrog__"]),
      attr(stanfit@sim$samples[[nChain]], "elapsed_time")
    )
  }

  nChains <- extract_n_chains(stanfit)
  do.call(rbind, lapply(seq_len(nChains), fun, stanfit))
}

get_diagnose_parameters <- function(stanfit, trueParameters = NULL, pars = NULL, ...) {
  paramNames <- if (is.null(trueParameters)) { pars } else { names(trueParameters) }
  paramFit   <-
    if (is.null(paramNames)) {
      extract(stanfit, permuted = FALSE)
    } else {
      extract(stanfit, pars = paramNames, permuted = FALSE)
    }
  paramNames <- dimnames(paramFit)[[3]]

  fun <- function(nChain, paramNames, stanfit) {
    monitorFit <- monitor(paramFit[, nChain, , drop = FALSE], print = FALSE, ...)

    if (!is.null(trueParameters)) {
      rankFit  <- sapply(paramNames, function(paramName) {
        c(
          true = as.numeric(trueParameters[paramName]),
          rank = prank(trueParameters[paramName], paramFit[, nChain, paramName])
        )
      })

      return(cbind(monitorFit, t(rankFit)))
    }

    monitorFit
  }

  nChains <- extract_n_chains(stanfit)
  do.call(rbind, lapply(seq_len(nChains), fun, paramNames, stanfit))
}

get_diagnose_ppredictive <- function(stanfit) {
  y     <- extract_y(stanfit)
  yPred <- extract_ypred(stanfit, permuted = FALSE)
  dim(yPred) <- c(dim(yPred)[1:2], stanfit@par_dims$ypred)
  # ^ Extract flattens yPred[iters, chains, T, R] into yPred[iters, chains, T*R]

  innerFun <- function(y, yPred) {
    yPredKS <- suppressWarnings(
      apply(yPred, 1, function(x) { ks.test(x, y)$statistic } )
    )

    predictiveRankApply <- function(y, yPred, fun, ...) {
      prank(fun(y, ...), apply(yPred, 1, fun, ...))
    }

    c(
      y25Rank  = predictiveRankApply(y, yPred, quantile, probs = 0.25),
      yMedRank  = predictiveRankApply(y, yPred, median),
      y75Rank  = predictiveRankApply(y, yPred, quantile, probs = 0.75),
      yMeanRank = predictiveRankApply(y, yPred, mean),
      ksMean   = mean(yPredKS),
      ksMedian = median(yPredKS)
    )
  }

  outerFun <- function(nChain, y, yPred) {
    l <- lapply(
      seq_len(NCOL(y)),
      function(r) {
        innerFun(y[, r], yPred[, nChain, , r])
      }
    )

    names(l) <- sprintf("r%d", 1:NCOL(y))

    do.call(c, l)
  }

  nChains <- extract_n_chains(stanfit)
  do.call(rbind, lapply(seq_len(nChains), outerFun, y, yPred))
}
