TransitionSoftmax <- function(sBeta = NULL, bounds = list(NULL, NULL),
                           trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  LinkDensity(
    "TransitionSoftmax",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

freeParameters.TransitionSoftmax <- function(x) {
  sBetaStr <-
    if (is.Density(x$sBeta)) {
      sBetaBoundsStr <- make_bounds(x, "sBeta")
      sprintf(
        "
        matrix%s[K, S] sBeta[K];        // transition model regressors
                                        // sBeta[to, from, s regressors]
        ",
        sBetaBoundsStr
      )
    } else {
      ""
    }

  sBetaStr
}

fixedParameters.TransitionSoftmax <- function(x) {
  warning("fixedParameters.Softmax: TO BE IMPLEMENTED.")
  return("")
}

generated.TransitionSoftmax <- function(x) {
  ""
}

getParameterNames.TransitionSoftmax <- function(x) {
  return("sBeta")
}

logLike.TransitionSoftmax <- function(x) {
  "
  "
}

link.TransitionSoftmax <- function(x) {
  sprintf(
    "A[t, i] = softmax((s[t] * sBeta%s[i]')');",
    x$k
  )
}

prior.TransitionSoftmax <- function(x) {
  warning("prior.Softmax: TO BE IMPLEMENTED.")
  return("")
}
