explain         <- function(x, ...) { UseMethod("explain", x) }
parameters      <- function(x, ...) { UseMethod("parameters", x) }
prior           <- function(x, ...) { UseMethod("prior", x) }
loglike         <- function(x, ...) { UseMethod("loglike", x) }
generated       <- function(x, ...) { UseMethod("generated", x) }
getParameters   <- function(x, ...) { UseMethod("getParameters", x) }
is.multivariate <- function(x, ...) { UseMethod("is.multivariate", x) }

make_bounds <- function(x, name) {
  nameBound <- paste0(name, "Bounds")
  s <- ""
  if (!is.null(x[[nameBound]][[1]]) & !is.null(x[[nameBound]][[2]])) {
    s <- sprintf(
      "<lower = %s, upper = %s>",
      x[[nameBound]][[1]],
      x[[nameBound]][[2]]
    )
  } else {
    if (!is.null(x[[nameBound]][[1]])) {
      s <- sprintf("<lower = %s>", x[[nameBound]][[1]])
    }

    if (!is.null(x[[nameBound]][[2]])) {
      s <- sprintf("<upper = %s>", x[[nameBound]][[2]])
    }
  }
  return(s)
}

make_trunc <- function(x, name) {
  nameTrunc <- paste0(name, "trunc")
  s <- ""
  if (!is.null(x[[nameTrunc]][[1]]) & !is.null(x[[nameTrunc]][[2]])) {
    s <- sprintf(
      "T[%s, %s]",
      x[[nameTrunc]][[1]],
      x[[nameTrunc]][[2]]
    )
  } else {
    if (!is.null(x[[nameTrunc]][[1]])) {
      s <- sprintf("T[%s, ]", x[[nameTrunc]][[1]])
    }

    if (!is.null(x[[nameTrunc]][[2]])) {
      s <- sprintf("T[, %s]", x[[nameTrunc]][[2]])
    }
  }
  return(s)
}

Density <- function(name, ...) {
  dots <- list(...)[[1]]
  for (i in seq_along(dots)) {
    if (is.language(dots[[i]])) {
      dots[[i]] <- eval(dots[[i]])
    }
  }

  out  <- c(list(name = name), dots)
  structure(out, class = c(name, "Density"))
}

`+.Density` <- function(x, y) {
  if (is.Density(x))
    return(list(x, y))

  c(x, list(y))
}

is.Density <- function(x) {
  "Density" %in% class(x)
}

explain.Density <- function(x) {
  sprintf(
    "%s(%s)",
    x[1],
    paste(names(x[-1]), "=", x[-1], collapse = ", ")
  )
}

Gaussian <- function(mu = NULL, sigma  = NULL, bounds = list(NULL, NULL),
  muBounds = list(NULL, NULL), sigmaBounds = list(0, NULL),
  trunc  = list(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  # Density("Gaussian", as.list(match.call())[-1])
  Density(
    "Gaussian",
    mget(names(formals()), sys.frame(sys.nframe()))
  )
}

is.multivariate.Gaussian <- function(x) { FALSE }

parameters.Gaussian <- function(x) {
  muBoundsStr    <- make_bounds(x, "mu")
  sigmaBoundsStr <- make_bounds(x, "sigma")

  sprintf(
    "real%s mu%s%s;\nreal%s sigma%s%s;",
    muBoundsStr, x$k, x$r,
    sigmaBoundsStr, x$k, x$r
    )
}

prior.Gaussian <- function(x) {
  truncStr    <- make_trunc(x, "")
  sprintf("%s%s%s ~ normal(%s, %s) %s;", x$param, x$k, x$r, x$mu, x$sigma, truncStr)
}

loglike.Gaussian <- function(x) {
  index <- if (x$k == x$r & x$r == "") { "" } else
           if (x$k != "" & x$r == "") { sprintf("[%s]", x$k) } else
           {sprintf("[%s, %s]", x$k, x$r) }

  sprintf("loglike%s[t] = normal_lpdf(x[t] | mu%s%s, sigma%s%s);", index, x$k, x$r, x$k, x$r)
}

getParameters.Gaussian <- function(x) {
  return(list(mu = eval(x$mu), sigma = eval(x$sigma)))
}

MVGaussian <- function(mu = NULL, sigma  = NULL, bounds = c(NULL, NULL),
                       trunc  = c(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("MVGaussian", as.list(match.call())[-1])
}

is.multivariate.MVGaussian <- function(x) { TRUE }

parameters.MVGaussian <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

prior.MVGaussian <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

loglike.MVGaussian <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

getParameters.MVGaussian <- function(x) {
  stop("TO BE IMPLEMENTED.")
}

Beta <- function(alpha = NULL, beta = NULL, bounds = c(NULL, NULL),
                 trunc = c(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("Beta", as.list(match.call())[-1])
}

parameters.Beta <- function(x) {
  stop("You shouldn't be calling this")
}

prior.Beta <- function(x) {
  sprintf("%s%s%s ~ beta(%s, %s);", x$param, x$k, x$r, x$alpha, x$beta)
}

loglike.Beta <- function(x) {
  stop("You shouldn't be calling this")
}

is.multivariate.Beta <- function(x) { FALSE }

Dirichlet <- function(alpha = NULL, bounds = c(NULL, NULL),
                      trunc = c(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("Dirichlet", as.list(match.call())[-1])
}

parameters.Dirichlet <- function(x) {
  stop("You shouldn't be calling this")
}

prior.Dirichlet <- function(x) {
  sprintf("%s%s%s ~ dirichlet(%s);", x$param, x$k, x$r,
          sprintf("[%s]'", paste(eval(x$alpha), collapse = ", ")))
}

loglike.Dirichlet <- function(x) {
  stop("You shouldn't be calling this")
}

is.multivariate.Dirichlet <- function(x) { TRUE }

Fixed <- function(value = NULL, bounds = c(NULL, NULL),
                 trunc = c(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("Fixed", as.list(match.call())[-1])
}

parameters.Fixed <- function(x) {
  stop("You shouldn't be calling this")
}

prior.Fixed <- function(x) {
  sprintf("%s%s%s ~ normal(%s, 0.00000001);", x$param, x$k, x$r, x$value)
}

loglike.Fixed <- function(x) {
  stop("You shouldn't be calling this")
}

Default <- function(bounds = c(NULL, NULL),
                    trunc = c(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("Default", as.list(match.call())[-1])
}

parameters.Default <- function(x) {
  stop("You shouldn't be calling this")
}

prior.Default <- function(x) {
  sprintf("// %s%s%s ~ Default Stan priors;", x$param, x$k, x$r)
}

loglike.Default <- function(x) {
  stop("You shouldn't be calling this")
}
