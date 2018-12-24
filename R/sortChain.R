################################################################################
# This file contains very experimental stuff to fix label switching - BEWARE
################################################################################

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

order_block <- function(x, y, K) {
  epb   <- dim(y)[2] / K # elements per block
  P     <- permutations(K, K)
  xMed  <- apply(x, 2, median)
  block <- t(sapply(1:K, function(k) {1:epb + epb * (k - 1)}))
  if (dim(block)[1] == 1) { block <- t(block) }

  sqSum <- vector("numeric", nrow(P))
  for (p in 1:nrow(P)) {
    permInd  <- P[p, ]
    yPermMed <- apply(y[, block[permInd, ]], 2, median)
    sqSum[p] <- sum((xMed - yPermMed)^2)
  }

  P[which.min(sqSum), ]
}

sort_block <- function(x, y, K) {
  epb    <- dim(y)[2] / K # elements per block
  block  <- t(sapply(1:K, function(k) {1:epb + epb * (k - 1)})) # row = block
  if (dim(block)[1] == 1) { block <- t(block) }

  y[, t(block[order_block(x, y, K), ])]
}

sort_chain <- function(stanfit, reference, K) {
  x <-
    if (is.numeric(reference) && (reference %% 1 == 0)) {
      extract_obs_parameters(stanfit, permuted = FALSE, inc_warmup = TRUE)[, reference, ]
    } else {
      reference
    }
  y <- rstan::extract(stanfit, permuted = FALSE, inc_warmup = TRUE, pars = select_obs_parameters(stanfit))
  # extract_obs_parameters(stanfit, permuted = FALSE, inc_warmup = TRUE)

  nChains <- dim(y)[2]
  tmp     <- y
  for (nChain in seq_len(nChains)) {
    tmp[, nChain, ] <- sort_block(x, y[, nChain, ], K)
  }
  dimnames(tmp)[3][[1]] <- colnames(sort_block(x, y[, 1, ], K))

  tmp
}

stan_sort_chain <- function(stanfit, reference, K) {
  newSample <- sort_chain(stanfit, reference, K)
  newFit    <- stanfit

  nChains  <- dim(newSample)[2]
  nParams  <- dim(newSample)[3]
  for (nChain in seq_len(nChains)) {
    for (nParam in seq_len(nParams)) {
      paramName <- dimnames(newSample)[[3]][nParam]
      newFit@sim$samples[[nChain]][[paramName]] <- newSample[, nChain, paramName]
    }
  }

  newFit
}
