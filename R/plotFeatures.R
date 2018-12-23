# Internal functions to add features to plot ------------------------------
# This file contains all the internal functions for the features an user
# may add to many of the plots available in these package. We regret to
# say that these functions are currently undocumented :(.

#' @importFrom grDevices col2rgb dev.off n2mfrow rgb
#' @importFrom graphics abline axis boxplot hist legend lines par plot points polygon rect segments title
#' @importFrom stats density ks.test median quantile
#' @importFrom utils browseURL glob2rx head tail
NULL

add_colored_lines <- function(x, y, col, ...) {
  segments(
    x0  = head(x, -1), # First T-1 obs
    y0  = head(y, -1),
    x1  = tail(x, -1), # Trailing T-1 obs
    y1  = tail(y, -1),
    col = col,
    ...
  )
}

add_fan <- function(x, upper, lower, bgCol, lineCol, ...) {
  polygon(
    x   = c(x, rev(x)),
    y   = c(lower, rev(upper)),
    col = bgCol,
    ...
  )

  lines(x, lower, col = lineCol)
  lines(x, upper, col = lineCol)
}

add_legend_overlay <- function(...) {
  opar <- par(
    fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE
  )
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend(...)
  par(opar)
}

add_shade <- function(x, bgCol, ...) {
  theme <- getOption("BayesHMM.theme")

  rect(
    xleft   = head(x, -1),
    ybottom = get_ybottom(),
    xright  = tail(x, -1),
    ytop    = get_ytop(),
    col     = col2rgb_alpha(bgCol, theme$shadeAlpha),
    border  = NA,
    ...
  )
}

add_title_overlay <- function(...) {
  opar <- par(
    fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE
  )
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  title(...)
  par(opar)
}

add_features <- function(x, y = NULL, z = NULL, p = NULL, pInt = NULL, k = NULL,
                         features = NULL) {
  theme <- getOption("BayesHMM.theme")
  kCol <- theme$states
  zCol <- kCol[z]

  if ("stateShade" %in% features) {
    add_shade(x, zCol)
  }

  if ("probabilityFan" %in% features) {
    add_fan(x, upper = pInt[2, ], lower = pInt[1, ], kCol[k], kCol[k])
  }

  if ("yColoredLine" %in% features) {
    add_colored_lines(x, y, zCol)
  }

  if ("yColoredDots" %in% features) {
    points(x, y, pch = 21, bg = zCol, col = zCol)
  }

  if ("probabilityColoredLine" %in% features) {
    add_colored_lines(x, p, zCol)
  }

  if ("probabilityColoredDots" %in% features) {
    points(x, p, pch = 21, bg = zCol, col = zCol)
  }

  if ("bottomColoredMarks" %in% features) {
    points(x, rep(par()$usr[3], length(x)), pch = "|", bg = zCol, col = zCol)
  }

  if ("topColoredMarks" %in% features) {
    points(x, rep(par()$usr[4], length(x)), pch = "|", bg = zCol, col = zCol)
  }
}
