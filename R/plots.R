plot_series <- function(stanfit, r = NULL, stateProbability = "smoothed", features = NULL,
                        stateFun = "mean", main = NULL, xlab = NULL, ylab = NULL, addLegend = TRUE, legend.cex = 1, ...) {
  stateProbability <- match.arg(
    stateProbability,
    choices = c("filtered", "smoothed", "viterbi"),
    several.ok	= FALSE
  )

  features <- match.arg(
    features,
    choices = c(
      "stateShade", "yColoredLine", "yColoredDots",
      "bottomColoredMarks", "topColoredMarks"
    ),
    several.ok	= TRUE
  )

  y    <- extract_y(stanfit)
  tidx <- seq_len(get_dim(y)[1])
  zFun <- switch(
    stateProbability,
    filtered = extract_alpha,
    smoothed = extract_gamma,
    viterbi  = extract_zstar
  )
  z    <- apply(zFun(stanfit, fun = stateFun), 2, which.max)
  K    <- extract_K(stanfit)
  R    <- extract_R(stanfit)
  rSeq <- if (is.null(r)) { seq_len(R) } else { r }

  opar <- par(
    oma   = c(2, 0, 0, 0),
    mfrow = rev(n2mfrow(length(rSeq)))
  )
  on.exit(par(opar))

  for (r in rSeq) {
    plot(
      NULL,
      main = if (is.null(main)) { "" } else { main },
      xlab = if (is.null(xlab)) { "Time" } else { xlab },
      ylab = if (is.null(ylab)) { if (R == 1) { "Observation" } else { sprintf("Variable %d", r) } } else { ylab },
      xlim = quantile(tidx, probs = c(0, 1)),
      ylim = quantile(y, probs = c(0, 1))
    )

    lines(x = tidx, y = y[, r])
    add_features(tidx, y[, r], z, features = features)
  }

  add_legend_overlay(
    x      = "bottom",
    legend = sprintf("Hidden state %d", 1:K),
    bty    = "n",
    horiz  = TRUE,
    fill   = getOption("BayesHMM.colors.clusters")[1:K],
    border = getOption("BayesHMM.colors.clusters")[1:K],
    cex    = legend.cex
  )
}

plot_state_probability <- function(stanfit, stateProbability = "smoothed", features = NULL,
                                   stateProbabilityFun = "mean", stateProbabilityInterval = NULL,
                                   main = NULL, xlab = NULL, ylab = NULL, addLegend = TRUE, legend.cex = 1, ...) {

  stateProbability <- match.arg(
    stateProbability,
    choices = c("filtered", "smoothed", "viterbi"),
    several.ok	= FALSE
  )

  features <- match.arg(
    features,
    choices = c(
      "probabilityColoredLine", "probabilityColoredDots", "probabilityFan",
      "stateShade", "bottomColoredMarks", "topColoredMarks"
    ),
    several.ok	= TRUE
  )

  zFun <- switch(
    stateProbability,
    filtered = extract_alpha,
    smoothed = extract_gamma,
    viterbi  = extract_zstar
  )
  z    <- apply(zFun(stanfit, fun = stateProbabilityFun), 2, which.max)
  p    <- zFun(stanfit, fun = stateProbabilityFun)
  pInt <- zFun(stanfit, fun = stateProbabilityInterval)
  tidx <- seq_len(get_dim(z)[1])
  K    <- extract_K(stanfit)

  opar <- par(
    oma   = c(6, 0, 0, 0),
    mfrow = c(K, 1)
  )
  on.exit(par(opar))

  for (k in 1:K) {
    opar <- par(
      mar = par_edit(
        par()$mar,
        if (k != K) { 0 },
        NULL,
        if (k != 1) { 0 },
        NULL
      )
    )

    plot(
      x    = tidx,
      y    = p[k, ],
      main = if (k == 1) { main },
      xlab = if (k == K) { xlab } else { "" },
      ylab = sprintf("Hidden state %d", k),
      type = "l",
      col  = getOption("BayesHMM.colors.clusters")[k],
      xaxt = "n",
      yaxt = "n",
      ...
    )

    add_features(tidx, y[, 1], z, p = p[k, ], pInt = pInt[, k, ], k, features)

    axis(if (k %% 2) { 4 } else { 2 })

    if (k == K) { axis(1) }
  }
  par(opar)

  if (addLegend) {
    add_legend_overlay(
      x      = "bottom",
      legend = sprintf("Hidden state %d", 1:K),
      bty    = "n",
      horiz  = TRUE,
      fill   = getOption("BayesHMM.colors.clusters")[1:K],
      border = getOption("BayesHMM.colors.clusters")[1:K],
      cex    = legend.cex
    )
  }
}

plot_params <- function() {

}

plot_ppredictive <- function(stanfit, type = "", r = NULL, subset = NULL,
                             fun = NULL, fun1 = NULL, fun2 = NULL,
                             densityControl = NULL, cumulativeControl = NULL,
                             funControl = NULL, fun1Control = NULL, fun2Control = NULL,
                             boxplotControl = NULL, main = NULL) {
  R     <- extract_R(stanfit)
  y     <- extract_y(stanfit)
  yPred <- extract_ypred(stanfit)
  if (!is.null(subset)) { yPred <- yPred[subset, , , drop = FALSE] }

  rSeq  <- if (is.null(r)) { seq_len(R) } else { r }

  funList <- list(
    density    = function(y, yPred) {
      plot_ppredictive_density(y, yPred, densityControl, NULL, NULL, NULL, FALSE)
    },
    cumulative = function(y, yPred) {
      plot_ppredictive_cumulative(y, yPred, cumulativeControl, NULL, NULL, NULL, FALSE)
    },
    hist       = function(y, yPred) {
      plot_ppredictive_hist(y, yPred, fun, funControl, NULL, NULL, NULL, FALSE)
    },
    boxplot    = function(y, yPred) {
      plot_ppredictive_boxplot(y, yPred, boxplotControl, NULL, NULL, NULL, FALSE)
    },
    scatter    = function(y, yPred) {
      plot_ppredictive_scatter(y, yPred, fun1, fun2, fun1Control, fun2Control, NULL, NULL, NULL, FALSE)
    },
    ks         = function(y, yPred) {
      plot_ppredictive_ks(y, yPred, NULL, NULL, NULL, NULL, FALSE)
    }
  )

  funSeq <- funList[which(names(funList) %in% type)]

  opar   <- par(
    oma   = c(2, 0, 0, 0),
    mfrow = rev(n2mfrow(length(rSeq) * length(funSeq)))
  )
  on.exit(par(opar))

  for (r in rSeq) {
    for (f in funSeq) {
      f(y[, r], yPred[, , r])
      if (R != 1) {
        title(sprintf("Variable %d", r), adj = 1, line = 0.5, cex.main = 1)
      }
    }
  }

  add_title_overlay(
    main = if (is.null(main)) { "Posterior Predictive Checks" } else { main },
    line = -1.5
  )

  add_legend_overlay(
    x = "bottom",
    legend = c("Observed sample", "Posterior predictive samples"),
    border = c("black", "gray80"),
    fill   = c("black", "gray80"),
    bty    = "n",
    horiz  = TRUE
  )
}

plot_ppredictive_scatter <- function(y, yPred, fun1 = NULL, fun2 = NULL,
                                             control1 = NULL, control2 = NULL, main = NULL,
                                             xlab = NULL, ylab = NULL, addLegend = TRUE) {

  myFun <- function(x) {
    if (is.null(control1)) { control1 <- list() }
    if (is.null(control2)) { control2 <- list() }
    cbind(
      do.call(fun1, c(list(x = x), control1)),
      do.call(fun2, c(list(x = x), control2))
    )
  }

  yFun     <- myFun(y)
  yPredFun <- t(apply(yPred, 1, myFun))
  yAll         <- rbind(yFun, yPredFun)

  plot(
    NULL,
    main = if (is.null(main)) { "" } else { main },
    xlab = if (is.null(xlab)) { funinvarName(fun1) } else { xlab },
    ylab = if (is.null(ylab)) { funinvarName(fun2) } else { ylab },
    xlim = quantile(yAll[, 1], probs = c(0, 1)),
    ylim = quantile(yAll[, 2], probs = c(0, 1))
  )

  points(
    yPredFun,
    pch = 21,
    col = "gray80",
    bg  = "gray80"
  )

  points(
    yFun,
    pch = 21,
    bg  = "black",
    col = "black"
  )

  if (addLegend) {
    legend(
      x = "topright",
      legend = c("Observed sample", "Posterior predictive samples"),
      pch = 21,
      bg  = c("black", "gray80"),
      col = c("black", "gray80"),
      bty = "n"
    )
  }
}

plot_ppredictive_hist <- function(y, yPred, fun, control = NULL, main = NULL,
                                     xlab = NULL, ylab = NULL, addLegend = TRUE) {

  myFun <- function(x) {
    if (is.null(control)) { control <- list() }
    do.call(fun, c(list(x = x), control))
  }

  yFun     <- myFun(y)
  yPredFun <- apply(yPred, 1, myFun)

  hist(
    yPredFun,
    main = if (is.null(main)) { "" } else { main },
    xlab = if (is.null(xlab)) { funinvarName(fun) } else { xlab },
    ylab = if (is.null(ylab)) { "Frequency" } else { ylab },
    breaks = "FD",
    col    = "gray80",
    border = "gray"
  )

  abline(v = yFun)

  if (addLegend) {
    legend(
      x = "topright",
      legend = c("Observed sample", "Posterior predictive samples"),
      lwd = 2,
      col = c("black", "gray80"),
      bty = "n"
    )
  }
}

plot_ppredictive_ks <- function(y, yPred, control = NULL, main = NULL,
                                xlab = NULL, ylab = NULL, addLegend = TRUE) {

  yPredFun <- suppressWarnings(
    apply(yPred, 1, function(x) { ks.test(x, y)$statistic } )
  )

  hist(
    yPredFun,
    main = if (is.null(main)) { "" } else { main },
    xlab = if (is.null(xlab)) { "Kolmogorov-Smirnov Statistic" } else { xlab },
    ylab = if (is.null(ylab)) { "Frequency" } else { ylab },
    breaks = "FD",
    col    = "gray80",
    border = "gray"
  )
}

plot_ppredictive_boxplot <- function(y, yPred, control = NULL, main = NULL,
                                     xlab = NULL, ylab = NULL, addLegend = TRUE) {

  argList <- list(
    x    = cbind(y, t(yPred)),
    main = if (is.null(main)) { "" } else { main },
    xlab = if (is.null(xlab)) { "Sample" } else { xlab },
    ylab = if (is.null(ylab)) { "Observation" } else { ylab },
    border = c("black", rep("gray80", nrow(yPred))),
    names  = c("y", seq_len(nrow(yPred)))
  )

  if (is.null(control)) { control <- list() }

  do.call(boxplot, c(argList, control))
}

plot_ppredictive_cumulative <- function(y, yPred, control = NULL, main = NULL,
                                        xlab = NULL, ylab = NULL, addLegend = TRUE) {
  qs <- seq(from = 0, to = 1, by = 0.01)
  myCumulative <- function(x) {
    if (is.null(control)) { control <- list() }
    do.call(quantile, c(list(x = x, probs = qs), control))
  }

  yCumulative     <- myCumulative(y)
  yPredCumulative <- t(apply(yPred, 1, myCumulative))
  xlim <- quantile(c(as.numeric(y), as.numeric(yPred)), probs = c(0, 1))

  plot(
    NULL,
    main = if (is.null(main)) { "" } else { main },
    xlab = if (is.null(xlab)) { "Cumulative Density" } else { xlab },
    ylab = if (is.null(ylab)) { "Observation" } else { ylab },
    xlim = xlim,
    ylim = c(0, 1)
  )

  for (yrow in seq_len(nrow(yPredCumulative))) {
    lines(x = yPredCumulative[yrow, ], y = qs, col = "gray80")
  }

  lines(x = yCumulative, y = qs, col = "black")

  if (addLegend) {
    legend(
      x = "bottomright",
      legend = c("Observed sample", "Posterior predictive samples"),
      lwd = 2,
      col = c("black", "gray80"),
      bty = "n"
    )
  }
}

plot_ppredictive_density <- function(y, yPred, control = NULL, main = NULL,
                                     xlab = NULL, ylab = NULL, addLegend = TRUE) {
  myDensity <- function(x) {
    if (is.null(control)) { control <- list(bw = "SJ") }
    do.call(density, c(list(x = x), control))
  }

  yDensity     <- myDensity(y)
  yPredDensity <- apply(yPred, 2, myDensity)
  yExtremes <- sapply(
    c(list(yDensity), yPredDensity), function(l) {
      c(
        quantile(l$x, probs = c(0, 1)),
        quantile(l$y, probs = c(0, 1))
      )
    }
  )

  plot(
    NULL,
    main = if (is.null(main)) { "" } else { main },
    xlab = if (is.null(xlab)) { "Observation" } else { xlab },
    ylab = if (is.null(ylab)) { "Density" } else { ylab },
    xlim = c(min(yExtremes[1, ]), max(yExtremes[2, ])),
    ylim = c(min(yExtremes[3, ]), max(yExtremes[4, ])),
    col  = "black"
  )

  for (yp in yPredDensity) {
    lines(yp, col = "gray80")
  }

  lines(yDensity, col = "black")

  if (addLegend) {
    legend(
      x = "topright",
      legend = c("Observed sample", "Posterior predictive samples"),
      lwd = 2,
      col = c("black", "gray80"),
      bty = "n"
    )
  }
}
