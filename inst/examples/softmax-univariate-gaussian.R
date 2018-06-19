library(rstan)

logsumexp <- function(x) {
  y = max(x)
  y + log(sum(exp(x - y)))
}

softmax <- function(x) {
  exp(x - logsumexp(x))
}

tvhmm_sim <- function(T, K, u, w, p.init) {
  m <- ncol(u)

  if (dim(u)[1] != T)
    stop("The input matrix must have T rows.")

  # if (any(dim(w) != c(K, m)))
  #   stop("The transition weight matrix must be of size Kxm, where m is the size of the input vector.")

  if (length(p.init) != K)
    stop("The vector p.init must have length K.")

  p.mat <- matrix(0, nrow = T, ncol = K)
  p.mat[1, ] <- p.init

  z <- vector("numeric", T)
  z[1] <- sample(x = 1:K, size = 1, replace = FALSE, prob = p.init)
  for (t in 2:T) {
    p.mat[t, ] <- softmax(sapply(1:K, function(j) {u[t, ] %*% w[[z[t - 1]]][j, ]}))
    z[t] <- sample(x = 1:K, size = 1, replace = FALSE, prob = p.mat[t, ])
  }

  x <- vector("numeric", T)
  for (t in 1:T) {
    x[t] <- rnorm(1, 10 * z[t], 1)
  }

  list(
    u = u,
    z = z,
    x = x,
    p.mat = p.mat
  )
}

mySpec <- hmm(
  K = 2, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = Default(),
  transition  = Softmax(
    sBeta = Gaussian(0, 5)
  ),
  name = "TVHMM Softmax Univariate Gaussian"
)

is.TVTransition(mySpec)

set.seed(9000)
s <- cbind(
  rep(1, 300),
  rnorm(300)
)

xBeta <- list(
  matrix(
    c(0.5, 0.3, -0.5, -0.3),

    ncol = 2, nrow = 2
  ),
  matrix(
    c(-0.9, -1.0, 0.6, 0.7),
    ncol = 2, nrow = 2
  )
)

tmp <- tvhmm_sim(300, 2, s, xBeta, p.init = c(0.5, 0.5))

myData <- list(
  y = as.matrix(tmp$x),
  T = NROW(tmp$x),
  K = 2,
  S = 2,
  s = s,
  R = 1
)

browseURL(write_model(mySpec, noLogLike = FALSE, "out"))

myFit <- run(mySpec, data = myData, chains = 1, iter = 500, writeDir = "out")

rstan::plot(myFit, pars = c("mu11", "mu21", "mu31"))

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)
