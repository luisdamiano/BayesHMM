get_cluster_colors <- function(K) {
  cols <- getOption("BayesHMM.clusterColors")
  if (length(cols) < K) {
    stop("Please, use options to set a larger pallete in BayesHMM.clusterColors")
  }
  cols[1:K]
}

set_layout <- function(K) {
  n2mfrow(K)
}

par_reset <- function() {
  invisible(tryCatch({dev.off()}, error = function(e) { }))
}

add_fan <- function(tidx, upper, lower, bgCol, lineCol) {
  polygon(
    x   = c(rev(tidx), tidx),
    y   = c(lower, upper),
    col = bgCol
  )

  lines(tidx, lower, col = lineCol)
  lines(tidx, upper, col = lineCol)
}

col2rgb_alpha <- function(bgCol, alpha = 1) {
  if (alpha >= 0 && alpha <= 1) {
    alpha <- alpha * 255
  } else if (alpha < 0 || alpha > 255) {
    stop("Not a valid entry for alpha (transparency).")
  }

  apply(
    col2rgb(bgCol, alpha = FALSE),
    2,
    function(tidx) {
      rgb(tidx[1], tidx[2], tidx[3], alpha = alpha, maxColorValue = 255)
    }
  )
}

add_shade <- function(tidx, bgCol) {
  rect(
    xleft   = head(tidx, -1),
    ybottom = get_ybottom(),
    xright  = tail(tidx, -1),
    ytop    = get_ytop(),
    col     = col2rgb_alpha(bgCol, getOption("BayesHMM.colors.shadeAlpha")),
    border  = NA
  )
}

add_colored_lines <- function(tidx, y, col, ...) {
  segments(
    x0  = head(tidx, -1), # First T-1 obs
    y0  = head(y, -1),
    x1  = tail(tidx, -1), # Trailing T-1 obs
    y1  = tail(y, -1),
    col = col,
    ...
  )
}

add_features <- function(tidx, y = NULL, z = NULL, p = NULL, pInt = NULL, k = NULL, features = NULL) {
  kCol <- getOption("BayesHMM.colors.clusters")
  zCol <- kCol[z]

  if ("stateShade" %in% features) {
    add_shade(tidx, zCol)
  }

  if ("probabilityFan" %in% features) {
    add_fan(tidx, upper = pInt[2, ], lower = pInt[1, ], kCol[k], kCol[k])
  }

  if ("yColoredLine" %in% features) {
    add_colored_lines(tidx, y, zCol)
  }

  if ("yColoredDots" %in% features) {
    points(tidx, y, pch = 21, bg = zCol, col = zCol)
  }

  if ("probabilityColoredLine" %in% features) {
    add_colored_lines(tidx, p, zCol)
  }

  if ("probabilityColoredDots" %in% features) {
    points(tidx, p, pch = 21, bg = zCol, col = zCol)
  }

  if ("bottomColoredMarks" %in% features) {
    points(tidx, rep(par()$usr[3], length(y)), pch = "|", bg = zCol, col = zCol)
  }

  if ("topColoredMarks" %in% features) {
    points(tidx, rep(par()$usr[4], length(y)), pch = "|", bg = zCol, col = zCol)
  }
}

get_dim <- function(x) {
  if (is.null(dim(x))) {
    length(x)
  } else {
    dim(x)
  }
}

get_ytop    <- function() { par()$usr[4] }
get_ybottom <- function() { par()$usr[3] }

par_edit    <- function(par, ...) {
  dots  <- list(...)
  for (i in seq_len(length(dots))) {
    if (!is.null(dots[[i]])) {
      par[i] <- dots[[i]]
    }
  }
  par
}

add_legend  <- function(p, ...) {
  opar <- par(no.readonly = TRUE)
  par(
    mar = c(0, 0, 0, 0),
    mai = c(0, 0, 0, 0)
    )
  plot.new()
  legend(...)
  par(opar)
}

plot_series <- function(stanfit, state = "smoothed", features = NULL,
                        stateSummary = "mean", main = NULL, xlab = NULL, legend = TRUE, legend.cex = 1, ...) {
  state <- match.arg(
    state,
    choices = c("filtered", "smoothed", "viterbi"),
    several.ok	= FALSE
  )

  features <- match.arg(
    features,
    choices = c(
      "none", "stateShade", "yColoredLine", "yColoredDots",
      "bottomColoredMarks", "topColoredMarks"
    ),
    several.ok	= TRUE
  )

  y    <- extract_y(stanfit)
  tidx <- seq_len(get_dim(y)[1])
  zFun <- switch(
    state,
    filtered = extract_alpha,
    smoothed = extract_gamma,
    viterbi  = extract_zstar
  )
  z    <- apply(zFun(stanfit, summary = stateSummary), 2, which.max)
  K    <- extract_K(stanfit)

  par_reset()
  layout(mat = 1:2, heights = c(0.9, 0.1))
  opar <- par(no.readonly = TRUE)
  par(
    mar = par_edit(
      opar$mar,
      if (is.null(xlab)) { 1 } else { 4 },
      NULL,
      if (is.null(main)) { 1 },
      NULL
    )
  )
  plot(
    x    = tidx,
    y    = y,
    main = main,
    xlab = xlab,
    type = "l",
    col  = getOption("BayesHMM.colors.yLine"),
    ...
  )
  # par(opar)

  add_features(tidx, y, z, features)

  if (legend) {
    add_legend(
      x      = "center",
      legend = sprintf("Hidden state %d", 1:K),
      bty    = "n",
      horiz  = TRUE,
      fill   = getOption("BayesHMM.colors.clusters")[1:K],
      border = getOption("BayesHMM.colors.clusters")[1:K],
      cex    = legend.cex
    )
  }
}

plot_prob <- function(stanfit, state = "smoothed", features = NULL,
                      stateSummary = "mean", probInterval = NULL,
                      main = NULL, xlab = "", legend = TRUE, legend.cex = 1, ...) {

  state <- match.arg(
    state,
    choices = c("filtered", "smoothed", "viterbi"),
    several.ok	= FALSE
  )

  features <- match.arg(
    features,
    choices = c(
      "none", "stateShade", "probabilityColoredLine", "probabilityColoredDots",
      "bottomColoredMarks", "topColoredMarks", "probabilityFan"
    ),
    several.ok	= TRUE
  )

  zFun <- switch(
    state,
    filtered = extract_alpha,
    smoothed = extract_gamma,
    viterbi  = extract_zstar
  )
  z    <- apply(zFun(stanfit, summary = stateSummary), 2, which.max)
  p    <- zFun(stanfit, summary = stateSummary)
  pInt <- zFun(stanfit, summary = probInterval)
  tidx <- seq_len(get_dim(z)[1])
  K    <- extract_K(stanfit)

  par_reset()
  opar <- par(no.readonly = TRUE)
  layout(1:(K + 1), heights = c(rep(0.90 / K, K), 0.10))
  for (k in 1:K) {
    par(
      mar = par_edit(
        opar$mar,
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

    add_features(tidx, y, z, p = p[k, ], pInt = pInt[, k, ], k, features)

    axis(if (k %% 2) { 4 } else { 2 })

    if (k == K) { axis(1) }
  }
  par(opar)

  if (legend) {
    add_legend(
      x      = "center",
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

plot_ppcheck <- function() {

}
