#' Validate a model via a procedure based on simulated data.
#'
#' Validation protocol inspired on Simulation Based Calibration (cite). Also known as fake data.
#'
#' \enumerate{
#'   \item Compile the prior predictive model (i.e. no likelihood statement in the Stan code).
#'   \item Draw \eqn{N} samples of the parameter vector \eqn{\theta} and the observation vector \eqn{\strong{y}_t} from prior predictive density.
#'   \item Compile the posterior predictive model (i.e. Stan code includes both prior density and likelihood statement).
#'   \item For all \eqn{n \in 1, \dots, N}:
#'   \enumerate{
#'     \item Feed \eqn{\strong{y}_t^{(n)}} to the full model.
#'     \item Draw one posterior sample of the observation variable \eqn{\strong{y_t}^{(n)}_{\text{new}}}.
#'     \item Collect Hamiltonian Monte Carlo diagnostics: number of divergences, number of times max tree depth is reached, maximum leapfrogs, warm up and sample times.
#'     \item Collect posterior sampling diagnostics: posterior summary measures (mean, sd, quantiles), comparison against the true value (rank), MCMC convergence measures (Monte Carlo SE, ESS, R Hat).
#'     \item Collect posterior predictive diagnostics: observation ranks, Kolmogorov-Smirnov statistic for observed sample vs posterior predictive samples.
#'   }
#' }
#'
#' @param spec A specification object returned by \code{\link{specify}}.
#' @param N An integer with the number of repetitions of the validation protocol.
#' @param T An optional integer with the length of the time series to be simulated. It defaults to 1000 observations.
#' @param x An optional numeric matrix with covariates for Markov-switching regression. It defaults to NULL (no covariates).
#' @param seed An optional integer with the seed used for the simulations. It defaults to 9000.
#' @param nCores An optional integer with the number of cores to use to run the protocol in parallel. It defaults to half the number of available cores
#' @param ... Arguments to be passed to \code{\link{sampling}}.
#' @return A named list with two elements. The first element \emph{chains} is a data.frame with Markov-chain Monte Carlo convergence diagnostics (number of divergences, number of times max tree depth is reached, maximum leapfrogs, warm up and sampling times) and posterior predictive checks (observation ranks, Kolmogorov-Smirnov statistic for observed sample vs posterior predictive samples). The second element, \emph{parameters}, compare true versus estimated values for the unknown quantities (mean, sd, quantiles and other posterior measures, Monte Carlo standard error, estimated sample size, R Hat, and rank).
#' @export
#' @examples
validate_calibration <- function(spec, N, T = 1000, x = NULL, seed = 9000, nCores = NULL, ...) {
  dots <- list(...)

  # 1. Draw a sample from the prior predictive density
  mySim      <- do.call(
    sim,
    list(spec, seed = seed, x = x, T = T, iter = max(N + 1, 500), chains = 1)
  )
  ySim       <- rstan::extract(mySim, pars = "ypred", permuted = FALSE, inc_warmup = FALSE)
  dim(ySim) <- c(dim(ySim)[1:2], mySim@par_dims$ypred)
  # ^ Extract flattens yPred[iters, chains, T, R] into yPred[iters, chains, T*R]
  paramSim   <- extract_obs_parameters(mySim, combine = cbind)
  rm(mySim); gc()

  # 2. Fit the model to simulated dataset
  myModel    <- compile(spec)

  if (is.null(nCores)) { nCores <- parallel::detectCores() / 2 }
  cl <- parallel::makeCluster(nCores, outfile = "")
  doParallel::registerDoParallel(cl)
  `%dopar%` <- foreach:::`%dopar%`
  l <- foreach::foreach(n = 1:N, .packages = c("BayesHMM", "rstan")) %dopar% {
    y          <- ySim[n, 1, , ]  # Chain 1
    paramTrue  <- paramSim[n, ] # paramSim[n, , ]
    myFit      <- do.call(
      sampling.Specification,   # sampling
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

  list(
    chains     = do.call(rbind.data.frame, lapply(l, `[[`, 1)),
    parameters = do.call(rbind.data.frame, lapply(l, `[[`, 2))
  )
}

# Undocumented internal methods

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
    d <- rstan::get_sampler_params(stanfit, inc_warmup = FALSE)[[nChain]]

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
      rstan::extract(stanfit, permuted = FALSE)
    } else {
      rstan::extract(stanfit, pars = paramNames, permuted = FALSE)
    }
  paramNames <- dimnames(paramFit)[[3]]

  fun <- function(nChain, paramNames, stanfit) {
    monitorFit <- rstan::monitor(paramFit[, nChain, , drop = FALSE], print = FALSE, ...)

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
  yPred <- extract_ypred(stanfit)
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
