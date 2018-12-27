#' Theme for BayesHMM visualizations and printouts
#'
#' BayesHMM comes with many built-in visualization and text printout methods to carry out bayesian data analysis out of the box. All plot colors and printout text formats are based on a fully-customizable theme loaded as a global setting. The theme is simply a named list with two elements \emph{BayesHMM.theme} and \emph{BayesHMM.print}, which in turn are named lists following the structure described below. Run \code{\link{get_default_theme}} to see the default values.
#'
#' \strong{BayesHMM.theme}: settings for visualizations.
#' \describe{
#'   \item{boxY}{A character string with the color of the lines of the box corresponding to the observed sample. Applies to \code{\link{plot_ppredictive}} with features \emph{boxplot}.}
#'   \item{boxYPred}{A character string with the color of the lines of the many boxes corresponding to the samples drawn from the posterior predictive density. Applies to \code{\link{plot_ppredictive}} with features \emph{boxplot}.}
#'   \item{cumulativeY}{A character string with the color of the cumulative density line for the observed sample. Applies to \code{\link{plot_ppredictive}} with features \emph{cumulative}.}
#'   \item{cumulativeYPred}{A character string with the color of the cumulative density lines for the many samples drawn from the posterior predictive density. Applies to \code{\link{plot_ppredictive}} with features \emph{cumulative}.}
#'   \item{densityY}{A character string with the color of the density line for the observed sample. Applies to \code{\link{plot_ppredictive}} with features \emph{density}.}
#'   \item{densityYPred}{A character string with the color of the density lines for the many samples drawn from the posterior predictive density. Applies to \code{\link{plot_ppredictive}} with features \emph{density}.}
#'   \item{histBorder}{A character string with the color of the border of the histogram bars. Applies to \code{\link{plot_ppredictive}} with features \emph{histogram}.}
#'   \item{histCol}{A character string with the color of the area of the histogram bars. Applies to \code{\link{plot_ppredictive}} with features \emph{histogram}.}
#'   \item{histLine}{A character string with the color of the vertical line related to the \emph{fun} argument of the \code{\link{plot_ppredictive}}) function. The line is located at the value returned by \emph{fun} applied to the actual sample. Applies to \code{\link{plot_ppredictive}} with features \emph{histogram}.}
#'   \item{ksBorder}{A character string with the color of the border of the histogram bars. Applies to \code{\link{plot_ppredictive}} with features \emph{ks}.}
#'   \item{ksCol}{A character string with the color of the area of the histogram bars. Applies to \code{\link{plot_ppredictive}} with features \emph{ks}.}
#'   \item{scatterY}{A character string with the color of the point for the observed sample. Applies to \code{\link{plot_ppredictive}} with features \emph{scatterplot}.}
#'   \item{scatterYPred}{A character string with the color of the points for the many samples drawn from the posterior predictive density. Applies to \code{\link{plot_ppredictive}} with features \emph{scatterplot}.}
#'   \item{seriesY}{A character string with the color of the observation time series. Applies to \code{\link{plot_series}} and \code{\link{plot_state_probability}}.}
#'   \item{shadeAlpha}{A numeric value between zero and one with the transparency setting for the shaded features, where lower values lead to more transparent shading. Applies to \code{\link{plot_series}} and \code{\link{plot_state_probability}}.}
#'   \item{states}{A vector of character strings with the colors of each latent state. Applies to \code{\link{plot_series}} and \code{\link{plot_state_probability}}.}
#' }
#'
#' Note that the overall legend uses the colors \emph{densityY} and \emph{densityYPred} for the actual sample and the posterior predictive samples respectively.
#'
#' \strong{BayesHMM.print}: settings for text printouts.
#' \describe{
#'   \item{char}{A character to use to print text lines and boxes. It defaults to an underscore.}
#'   \item{tab}{A character string to be used for indentation. It defaults to two spaces.}
#'   \item{textWidth}{An integer with the width of the text lines and boxes. It defaults to 80 characters.}
#' }
#' @name theme
#' @family visualization functions
#'
NULL

#' Loads a theme into the R session.
#'
#' @keywords internal
load_theme <- function() {
  # Look for config file in a few reasonable paths. If not found, load default.
  newOps <- get_default_theme()

  op    <- options()
  toset <- !(names(newOps) %in% names(op))
  if (any(toset)) options(newOps[toset])

  invisible()
}

#' Return the default theme.
#'
#' @return A named list with the theme.
#' @family visualization functions
get_default_theme <- function() {
  list(
    BayesHHM.config = "example",
    BayesHMM.print = list(
      char         = "_",
      tab          = "  ",
      textWidth    = 80
    ),
    BayesHMM.theme = list(
      boxY            = "black",
      boxYPred        = "gray80",
      cumulativeY     = "black",
      cumulativeYpred = "gray80",
      densityY        = "black",
      densityYPred    = "gray80",
      histBorder      = "gray",
      histCol         = "gray80",
      histLine        = "black",
      ksBorder        = "gray",
      ksCol           = "gray80",
      ksLine          = "black",
      scatterY        = "black",
      scatterYPred    = "gray80",
      seriesY         = "gray40",
      shadeAlpha      = 0.2,
      states          = c(
        "#E41F26", "#2EA147", "#1D79B4",
        # "#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF",
        "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"
      )
    )
  )
}

#' Return the current theme.
#'
#' @return A named list with the theme.
#' @family visualization functions
get_current_theme <- function() {
  list(
    BayesHMM.theme = getOption("BayesHMM.theme"),
    BayesHMM.print = getOption("BayesHMM.print")
  )
}

#' Return the current theme for visualizations.
#'
#' @return A named list with the theme for visualizations.
#' @keywords internal
get_plot_theme <- function() {
  getOption("BayesHMM.theme")
}

#' Return the current theme for text printouts.
#' @return A named list with the theme for text printouts..
#' @keywords internal
get_print_settings <- function() {
  getOption("BayesHMM.print")
}
