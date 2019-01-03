mySpec <- hmm(
  K = 2, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = c(1, 1)),
  transition  = TransitionSoftmax(
    uBeta = Gaussian(0, 10), P = 2
  ),
  name = "TVHMM Univariate Gaussian"
)

# Simulate dataset --------------------------------------------------------
logsumexp <- function(x) {
  y = max(x)
  y + log(sum(exp(x - y)))
}

softmax <- function(x) {
  exp(x - logsumexp(x))
}

tvhmm_sim <- function(T, K, u, w, p.init) {
  if (dim(u)[1] != T)
    stop("The input matrix must have T rows.")

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

  y <- vector("numeric", T)
  for (t in 1:T) {
    y[t] <- rnorm(1, 10 * z[t], 1)
  }

  list(
    u = u,
    z = z,
    y = y,
    p.mat = p.mat
  )
}

set.seed(8000)
u <- cbind(
  rep(1, 300),
  rnorm(300)
)

uBeta <- list(
  matrix(
    c(0.5, 0.3, -0.5, -0.3),
    ncol = 2, nrow = 2
  ),
  matrix(
    c(-0.9, -1.0, 0.6, 0.7),
    ncol = 2, nrow = 2
  )
)

# Fit model ---------------------------------------------------------------
dataset <- tvhmm_sim(300, 2, u, uBeta, p.init = c(0.5, 0.5))
myModel <- compile(mySpec)
myAll   <- optimizing(
  mySpec, myModel, y = dataset$y, u = dataset$u,
  nRun = 20, keep = "all", nCores = 10
)
myBest  <- extract_best(myAll)

# Since mu_k = 10 * k (see line # 43)
# then mu1 ~ 10, mu2 ~ 20 is the correct answer
extract_grid(myAll, pars = "mu")
extract_obs_parameters(myBest)
