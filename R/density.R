explain       <- function(x, ...) { UseMethod("explain", x) }
parameters    <- function(x, ...) { UseMethod("parameters", x) }
prior         <- function(x, ...) { UseMethod("prior", x) }
loglike       <- function(x, ...) { UseMethod("loglike", x) }
generated     <- function(x, ...) { UseMethod("generated", x) }
getParameters <- function(x, ...) { UseMethod("getParameters", x) }

Density <- function(name, ...) {
  dots <- list(...)[[1]]
  out  <- c(list(name = name), dots)
  structure(out, class = c(name, "Density"))
}

explain.Density <- function(x) {
  sprintf("Fallback. Explain not implemented for %s yet", x$name)
  ## Auto sprintf "{name} Density. Priors {autoname params from list :)}
}

Gaussian <- function(mu = NULL, sigma  = NULL, bounds = c(NULL, NULL),
  trunc  = c(NULL, NULL), k = NULL, r = NULL, param = NULL) {
  Density("Gaussian", as.list(match.call())[-1])
}

parameters.Gaussian <- function(x) {
  sprintf("real mu%s%s;\nreal<lower = 0> sigma%s%s;", x$k, x$r, x$k, x$r)
}

prior.Gaussian <- function(x) {
  sprintf("%s%s%s ~ normal(%s, %s);", x$param, x$k, x$r, x$mu, x$sigma)
}

loglike.Gaussian <- function(x) {
  index <- if (x$k == x$r & x$r == "") { "" } else
           if (x$k != "" & x$r == "") { sprintf("[%s]", x$k) } else
           { sprintf("[%s, %s]", x$k, x$r) }

  sprintf("loglike%s[t] = normal_lpdf(x[t] | mu%s%s, sigma%s%s);", index, x$k, x$r, x$k, x$r)
}

getParameters.Gaussian <- function(x) {
  return(list(mu = eval(x$mu), sigma = eval(x$sigma)))
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
