validate_calibration <- function(spec, N, T = 1000, x = NULL, seed = 9000, cores = NULL, ...) {
  dots <- list(...)

  # 1. Draw a sample from the prior predictive density
  mySim      <- do.call(sim, list(spec, seed = seed, x = x, T = T, iter = max(N + 1, 500), chains = 1))
  ySim       <- extract(mySim, pars = "ypred", permuted = FALSE, inc_warmup = FALSE)
  paramSim   <- extract_obs(mySim, permuted = FALSE)
  rm(mySim); gc()

  # 2. Fit the model to simulated dataset
  myModel    <- compile(spec)

  if (is.null(cores)) { cores <- parallel::detectCores() / 2 }
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)
  l <- foreach::foreach(n = 1:N, .combine = c, .packages = c("BayesHMM", "rstan")) %dopar% {
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

    myDiag     <- validate(myFit, trueParameters = paramTrue)
    myDiag$chains$seed  <- seed
    myDiag$chains$n     <- n
    myDiag$parameters$n <- n
    rm(myFit); gc()
    return(myDiag)
  }
  parallel::stopCluster(cl)

  sapply(unique(names(l)), function(name) {
    out <- do.call(rbind.data.frame, l[which(names(l) == name)])
    rownames(out) <- NULL
    out
  }, simplify = FALSE)
}

validate <- function(stanfit, pars = select_obs_parameters(stanfit), trueParameters = NULL) {
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
      y25Rank   = predictiveRankApply(y, yPred, quantile, probs = 0.25),
      yMedRank  = predictiveRankApply(y, yPred, median),
      y75Rank   = predictiveRankApply(y, yPred, quantile, probs = 0.75),
      yMeanRank = predictiveRankApply(y, yPred, mean),
      ksMean    = mean(yPredKS),
      ksMedian  = median(yPredKS)
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
