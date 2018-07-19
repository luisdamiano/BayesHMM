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
  initial     = Dirichlet(alpha = c(0.5, 0.5)),
  transition  = TransitionSoftmax(
    uBeta = Gaussian(0, 3), P = 2
  ),
  name = "TVHMM Univariate Gaussian"
)

set.seed(9000)
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

dataset <- tvhmm_sim(300, 2, u, uBeta, p.init = c(0.5, 0.5))
myModel <- compile(mySpec)
myBest  <- optimizing(mySpec, myModel, y = dataset$x, u = dataset$u, nRun = 10, keep = "best", as_vector = FALSE)
myAll   <- optimizing(mySpec, myModel, y = dataset$x, u = dataset$u, nRun = 10, keep = "all", nCores = 4)

extract_grid(myBest, pars = "mu")
extract_grid(myAll, pars = "mu")

extract_obs_parameters(myBest)

extract_quantity(myBest, pars = "mu")
extract_quantity(myBest, pars = "alpha")
extract_alpha(myBest)
extract_obs_parameters(myBest)

sapply(myAll, extract_quantity, pars = "mu")

plot(extract_zstar(myBest))

table(real = dataset$z, viterbi = extract_zstar(myBest))

table(real = dataset$z, filtered = apply(myBest$par$alpha, 2, which.max))

plot_obs(myBest)

# plot_state_probability(myBest, stateProbabilityFun = identity, features = "stateShade")

myBest$par$A[300, , ]
