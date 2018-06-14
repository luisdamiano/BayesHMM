MVStudent <- function(mu = NULL, sigma  = NULL, nu = NULL, bounds = list(NULL, NULL),
                       trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  MultivariateDensity(
    "MVStudent",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

freeParameters.MVStudent <- function(x) {
  muStr <-
    if (is.Density(x$mu)) {
      sprintf(
        "vector[R] mu%s;",
        x$k
      )
    } else {
      ""
    }

  sigmaStr <-
    if (is.Density(x$sigma)) {
      sprintf(
        "cov_matrix[R] sigma%s;",
        x$k
      )
    } else {
      ""
    }

  nuStr <-
    if (is.Density(x$nu)) {
      nuBoundsStr <- make_bounds(x, "nu")
      sprintf(
        "real%s nu%s;",
        nuBoundsStr, x$k
      )
    } else {
      ""
    }

  collapse(muStr, sigmaStr, nuStr)
}

fixedParameters.MVStudent <- function(x) {
  muStr <-
    if (is.Density(x$mu)) {
      ""
    } else {
      if (!check_vector(x$mu)) {
        stop(sprintf("If fixed, mu must be a vector of size R"))
      }

      sprintf(
        "vector[R] mu%s = %s';",
        x$k, vector_to_stan(x$mu)
      )
    }

  sigmaStr <-
    if (is.Density(x$sigma)) {
      ""
    } else {
      if (!check_matrix(x$sigma)) {
        stop("If fixed, sigma must be a matrix of size RxR")
      }

      sprintf(
        "matrix[R, R] sigma%s = %s;",
        x$k, matrix_to_stan(x$sigma)
      )
    }

  nuStr <-
    if (is.Density(x$nu)) {
      ""
    } else {
      if (!check_scalar(x$nu)) {
        stop("If fixed, nu must be a scalar.")
      }

      sprintf(
        "real nu%s = %s;",
        x$k, x$r, x$nu
      )
    }

  collapse(muStr, sigmaStr, nuStr )
}

generated.MVStudent <- function(x) {
  sprintf(
    "if(zpred[t] == %s) ypred[t] = multi_student_t_rng(nu%s, mu%s, sigma%s)';",
    x$k, x$k, x$k, x$k
  )
}

getParameterNames.MVStudent <- function(x) {
  return(c("mu", "sigma", "nu"))
}

logLike.MVStudent <- function(x) {
  sprintf(
    "loglike[%s][t] = multi_student_t_lpdf(y[t] | nu%s, mu%s, sigma%s);",
    x$k, x$k, x$k, x$k
  )
}

prior.MVStudent <- function(x) {
  stop("TO BE IMPLEMENTED.")
}
