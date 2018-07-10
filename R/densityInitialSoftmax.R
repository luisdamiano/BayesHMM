InitialSoftmax <- function(vBeta = NULL, Q = NULL, bounds = list(NULL, NULL),
                    trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  LinkDensity(
    "InitialSoftmax",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

freeParameters.InitialSoftmax <- function(x) {
  vBetaStr <-
    if (is.Density(x$vBeta)) {
      vBetaBoundsStr <- make_bounds(x, "vBeta")
      sprintf(
        "
        matrix%s[K, Q] vBeta;   // initial model regressors
                                // vBeta[state, Q regressors]
        ",
        vBetaBoundsStr
      )
    } else {
      ""
    }

  vBetaStr
}

fixedParameters.InitialSoftmax <- function(x) {
  warning("fixedParameters.Softmax: TO BE IMPLEMENTED.")
  return("")
}

generated.InitialSoftmax <- function(x) {
  ""
}

getParameterNames.InitialSoftmax <- function(x) {
  return("vBeta")
}

logLike.InitialSoftmax <- function(x) {
  "
  "
}

link.InitialSoftmax <- function(x) {
  sprintf(
    "pi = softmax((v%s' * vBeta%s)');",
    x$k, x$k
  )
}

prior.InitialSoftmax <- function(x) {
  warning("prior.Softmax: TO BE IMPLEMENTED.")
  return("")
}
