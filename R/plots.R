plot_series <- function(fit, r = NULL,
                        stateProbability = "smoothed", features = NULL,
                        stateFun = mean, chain = 1,
                        main = NULL, xlab = NULL, ylab = NULL,
                        addLegend = TRUE, legend.cex = 1, ...) {

  theme <- getOption("BayesHMM.theme")

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

  y    <- extract_y(fit)
  tidx <- seq_len(get_dim(y)[1])
  zFun <- switch(
    stateProbability,
    filtered = classify_alpha,
    smoothed = classify_gamma,
    viterbi  = classify_zstar
  )
  z    <- zFun(fit, reduce = stateFun, chain = chain)
  K    <- extract_K(fit)
  R    <- extract_R(fit)
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
      ylab =
        if (is.null(ylab)) {
          colnames(y)[r]
        } else {
          ylab
        },
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
    fill   = theme$states[1:K],
    border = theme$states[1:K],
    cex    = legend.cex
  )
}

plot_state_probability <- function(fit, stateProbability = "smoothed",
                                   features = NULL, stateProbabilityFun = mean,
                                   stateProbabilityInterval = NULL, chain = 1,
                                   main = NULL, xlab = NULL, ylab = NULL,
                                   addLegend = TRUE, legend.cex = 1, ...) {

  theme <- getOption("BayesHMM.theme")

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
    filtered = classify_alpha,
    smoothed = classify_gamma,
    viterbi  = classify_zstar
  )
  z    <- zFun(fit, reduce = stateProbabilityFun, chain = chain)

  zFun <- switch(
    stateProbability,
    filtered = extract_alpha,
    smoothed = extract_gamma,
    viterbi  = extract_zstar
  )
  p    <- zFun(fit, reduce = stateProbabilityFun, chain = chain)
  pInt <- zFun(fit, reduce = stateProbabilityInterval, chain = chain)
  tidx <- seq_len(get_dim(z)[1])
  K    <- extract_K(fit)

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  par(
    oma   = c(6, 0, 0, 0),
    mfrow = c(K, 1)
  )
  for (k in 1:K) {
    par(
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
      col  = theme$states[k],
      xaxt = "n",
      yaxt = "n",
      ...
    )

    add_features(tidx, y[, 1], z, p = p[k, ], pInt = pInt[, k, ], k, features)

    axis(if (k %% 2) { 4 } else { 2 })

    if (k == K) { axis(1) }
  }

  if (addLegend) {
    add_legend_overlay(
      x      = "bottom",
      legend = sprintf("Hidden state %d", 1:K),
      bty    = "n",
      horiz  = TRUE,
      fill   = theme$states[1:K],
      border = theme$states[1:K],
      cex    = legend.cex
    )
  }
}

plot_params <- function() {

}


