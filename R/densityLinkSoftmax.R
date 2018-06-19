Softmax <- function(sBeta = NULL, bounds = list(NULL, NULL),
                    trunc = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  LinkDensity(
    "Softmax",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

freeParameters.Softmax <- function(x) {
  sBetaStr <-
    if (is.Density(x$sBeta)) {
      sBetaBoundsStr <- make_bounds(x, "sBeta")
      sprintf(
        "
        matrix%s[K, S] sBeta[K];          // transition model regressors
                                          // sBeta[to, from, s regressors]
        ",
        sBetaBoundsStr
      )
    } else {
      ""
    }

  sBetaStr
}

fixedParameters.Softmax <- function(x) {
  warning("fixedParameters.Softmax: TO BE IMPLEMENTED.")
  return("")
}

generated.Softmax <- function(x) {
  ""
}

getParameterNames.Softmax <- function(x) {
  return("sBeta")
}

logLike.Softmax <- function(x) {
  "
  "
}

link.Softmax <- function(x) {
  "A[t, i] = softmax((s[t] * sBeta[i]')');"
}

prior.Softmax <- function(x) {
  warning("prior.Softmax: TO BE IMPLEMENTED.")
  return("")
}
