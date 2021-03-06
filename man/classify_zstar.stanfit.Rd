% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.R
\name{classify_zstar.stanfit}
\alias{classify_zstar.stanfit}
\title{Assign the hidden states to the most likely path (\emph{zstar}).}
\usage{
\method{classify_zstar}{stanfit}(fit, reduce = posterior_mode,
  chain = "all")
}
\arguments{
\item{fit}{An object returned by either \code{\link{draw_samples}} or \code{\link{optimizing}}.}

\item{reduce}{An optional function applied to the samples corresponding to one time step \emph{t}, one hidden state \emph{k}, and one chain \emph{m}. The observation at time step \emph{t} is then assigned to the hidden state with largest value. Note that the user needs to supply a function as an argument, and not a character string with the name of the function. This argument is not used for maximum a posteriori estimates returned by \code{\link{optimizing}} since there is only one scalar per quantity. It defaults to the \code{\link{posterior_mode}} function.}

\item{chain}{Either "all" or any integer number between 1 and the number of chains M. In the latter case, only the samples generated by the selected chain are considered. This argument is not used for maximum a posteriori estimates returned by \code{\link{optimizing}} since there is only one result. It defaults to "all".}
}
\value{
A numeric vector with size equal to the time series length \emph{T} with values from 1 to the number of hidden states \emph{K}.
}
\description{
This is the result of Viterbi algorithm, which considers the joint distribution of the states.
}
\keyword{internal}
