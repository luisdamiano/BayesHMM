InitialSoftmax <- function(sBeta = NULL, bounds = list(NULL, NULL),
                    trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  LinkDensity(
    "InitialSoftmax",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

freeParameters.InitialSoftmax <- function(x) {
  sBetaStr <-
    if (is.Density(x$sBeta)) {
      sBetaBoundsStr <- make_bounds(x, "sBeta")
      sprintf(
        "
        matrix%s[K, S_init] sBeta_init;   // initial model regressors
                                          // sBeta[state, s regressors]
        ",
        sBetaBoundsStr
      )
    } else {
      ""
    }

  sBetaStr
}

fixedParameters.InitialSoftmax <- function(x) {
  warning("fixedParameters.Softmax: TO BE IMPLEMENTED.")
  return("")
}

generated.InitialSoftmax <- function(x) {
  ""
}

getParameterNames.InitialSoftmax <- function(x) {
  return("sBeta")
}

logLike.InitialSoftmax <- function(x) {
  "
  "
}

link.InitialSoftmax <- function(x) {
  sprintf(
    "pi = softmax((s%s' * sBeta%s)');",
    x$k, x$k
  )
}

prior.InitialSoftmax <- function(x) {
  warning("prior.Softmax: TO BE IMPLEMENTED.")
  return("")
}
