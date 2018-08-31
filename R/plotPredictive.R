setGeneric("plot_ppredictive", function(stanfit, ...) {standardGeneric("plot_ppredictive")} )

plot_ppredictive.stanfit <- function(stanfit, type = "", r = NULL, subset = NULL, chain = 1,
                             fun = NULL, fun1 = NULL, fun2 = NULL,
                             densityControl = NULL, cumulativeControl = NULL,
                             funControl = NULL, fun1Control = NULL, fun2Control = NULL,
                             boxplotControl = NULL, main = NULL) {
  theme <- getOption("BayesHMM.theme")

  R     <- extract_R(stanfit)
  y     <- extract_y(stanfit)
  yPred <- extract_ypred(stanfit, chain = chain)
  if (R == 1) { dim(yPred) <- c(dim(yPred), 1) } # extract_ypred drops index for [iteration, chain, r] when r = 1
  if (!is.null(subset)) { yPred <- yPred[subset, , , drop = FALSE] }

  rSeq  <- if (is.null(r)) { seq_len(R) } else { r }

  funList <- list(
    density    = function(y, yPred) {
      plot_ppredictive_density(
        y, yPred, densityControl,
        NULL, NULL, NULL, FALSE
      )
    },
    cumulative = function(y, yPred) {
      plot_ppredictive_cumulative(
        y, yPred, cumulativeControl,
        NULL, NULL, NULL, FALSE
      )
    },
    hist       = function(y, yPred) {
      plot_ppredictive_hist(
        y, yPred, fun, funControl,
        NULL, NULL, NULL, FALSE
      )
    },
    boxplot    = function(y, yPred) {
      plot_ppredictive_boxplot(
        y, yPred, boxplotControl,
        NULL, NULL, NULL, FALSE
      )
    },
    scatter    = function(y, yPred) {
      plot_ppredictive_scatter(
        y, yPred, fun1, fun2, fun1Control, fun2Control,
        NULL, NULL, NULL, FALSE
      )
    },
    ks         = function(y, yPred) {
      plot_ppredictive_ks(
        y, yPred,
        NULL, NULL, NULL, NULL, FALSE
      )
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
        title(colnames(y)[r], adj = 1, line = 0.5, cex.main = 1)
        # title(sprintf("Variable %d", r), adj = 1, line = 0.5, cex.main = 1)
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
    border = c(theme$yDensity, theme$yPredDensity),
    fill   = c(theme$yDensity, theme$yPredDensity),
    bty    = "n",
    horiz  = TRUE
  )
}

setMethod("plot_ppredictive", "stanfit", plot_ppredictive.stanfit)

plot_ppredictive_scatter <- function(y, yPred, fun1 = NULL, fun2 = NULL,
                                     control1 = NULL, control2 = NULL,
                                     main = NULL, xlab = NULL, ylab = NULL,
                                     addLegend = TRUE) {

  theme <- getOption("BayesHMM.theme")

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
    col = theme$yPredDensity,
    bg  = theme$yPredDensity
  )

  points(
    yFun,
    pch = 21,
    bg  = theme$yDensity,
    col = theme$yDensity
  )

  if (addLegend) {
    legend(
      x = "topright",
      legend = c("Observed sample", "Posterior predictive samples"),
      pch = 21,
      bg  = c(theme$yDensity, theme$yPredDensity),
      col = c(theme$yDensity, theme$yPredDensity),
      bty = "n"
    )
  }
}

plot_ppredictive_hist <- function(y, yPred, fun, control = NULL, main = NULL,
                                  xlab = NULL, ylab = NULL,
                                  addLegend = TRUE) {

  theme <- getOption("BayesHMM.theme")

  myFun <- function(x) {
    if (is.null(control)) { control <- list() }
    do.call(fun, c(list(x = x), control))
  }

  yFun     <- myFun(y)
  yPredFun <- apply(yPred, 1, myFun)

  hist(
    yPredFun,
    main = if (is.null(main)) { "" } else { main },
    xlab = if (is.null(xlab)) { toproper(funinvarName(fun)) } else { xlab },
    ylab = if (is.null(ylab)) { "Frequency" } else { ylab },
    breaks = "FD",
    col    = theme$histCol,
    border = theme$histBorder
  )

  abline(v = yFun, col = theme$histLine)

  if (addLegend) {
    legend(
      x = "topright",
      legend = c("Observed sample", "Posterior predictive samples"),
      lwd = 2,
      col = c(theme$histLine, theme$histCol),
      bty = "n"
    )
  }
}

plot_ppredictive_ks <- function(y, yPred, control = NULL, main = NULL,
                                xlab = NULL, ylab = NULL, addLegend = TRUE) {

  theme <- getOption("BayesHMM.theme")

  yPredFun <- suppressWarnings(
    apply(yPred, 1, function(x) { ks.test(x, y)$statistic } )
  )

  hist(
    yPredFun,
    main = if (is.null(main)) { "" } else { main },
    xlab = if (is.null(xlab)) { "Kolmogorov-Smirnov Statistic" } else { xlab },
    ylab = if (is.null(ylab)) { "Frequency" } else { ylab },
    breaks = "FD",
    col    = theme$histCol,
    border = theme$histBorder
  )
}

plot_ppredictive_boxplot <- function(y, yPred, control = NULL, main = NULL,
                                     xlab = NULL, ylab = NULL,
                                     addLegend = TRUE) {

  theme <- getOption("BayesHMM.theme")

  argList <- list(
    x    = cbind(y, t(yPred)),
    main = if (is.null(main)) { "" } else { main },
    xlab = if (is.null(xlab)) { "Sample" } else { xlab },
    ylab = if (is.null(ylab)) { "Observation" } else { ylab },
    border = c(theme$boxY, rep(theme$boxYPred, nrow(yPred))),
    names  = c("y", seq_len(nrow(yPred)))
  )

  if (is.null(control)) { control <- list() }

  do.call(boxplot, c(argList, control))
}

plot_ppredictive_cumulative <- function(y, yPred, control = NULL, main = NULL,
                                        xlab = NULL, ylab = NULL,
                                        addLegend = TRUE) {
  theme <- getOption("BayesHMM.theme")

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
    lines(x = yPredCumulative[yrow, ], y = qs, col = theme$yPredDensity)
  }

  lines(x = yCumulative, y = qs, col = theme$yDensity)

  if (addLegend) {
    legend(
      x = "bottomright",
      legend = c("Observed sample", "Posterior predictive samples"),
      lwd = 2,
      col = c(theme$yDensity, theme$yPredDensity),
      bty = "n"
    )
  }
}

plot_ppredictive_density <- function(y, yPred, control = NULL, main = NULL,
                                     xlab = NULL, ylab = NULL,
                                     addLegend = TRUE) {
  theme <- getOption("BayesHMM.theme")

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
    lines(yp, col = theme$yPredDensity)
  }

  lines(yDensity, col = theme$yDensity)

  if (addLegend) {
    legend(
      x = "topright",
      legend = c("Observed sample", "Posterior predictive samples"),
      lwd = 2,
      col = c(theme$yDensity, theme$yPredDensity),
      bty = "n"
    )
  }
}
