## from http://tr.im/hH5A
# logsumexp <- function(x) {
#   y = max(x)
#   y + log(sum(exp(x - y)))
# }
#
# softmax <- function(x) {
#   exp(x - logsumexp(x))
# }

library(rstan)

mySpec <- hmm(
  K = 2, R = 1,
  observation = RegCategoricalSoftmax(
    xBeta = Default(),
    M     = 3,
    N     = 5 # different categories
  ),
  initial     = Dirichlet(alpha = c(0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5)),
  name = "Univariate Categorical Softmax Regression"
)

set.seed(9000)
x <- as.matrix(
  cbind(
    rep(1, 300),
    rnorm(300),
    rnorm(300)
  )
)
b1 <- matrix(
  c(-1, 0, 1, -3, 0, 3, -5, 0, 5, -7, 0, 7, -9, 0, 9),
  ncol = 3, nrow = 5, byrow = TRUE
)

b2 <- matrix(
  c( 1, 0,-1,  3, 0,-3,  5, 0,-5,  7, 0,-7,  9, 0,-9),
  ncol = 3, nrow = 5, byrow = TRUE
)

y <- as.matrix(
  c(
    apply(
      exp(x[  1:150, ] %*% t(b1)) / rowSums(exp(x[  1:150, ] %*% t(b1))),
      1,
      function(x) { sample(1:5, size = 1, replace = FALSE, prob = x) }
    ),
    apply(
      exp(x[151:300, ] %*% t(b1)) / rowSums(exp(x[151:300, ] %*% t(b2))),
      1,
      function(x) { sample(1:5, size = 1, replace = TRUE, prob = x) }
    )
  )
)

myData  <- make_data(
  spec  = mySpec,
  y     = y,
  xBeta = x
)

myFit <- run(mySpec, data = myData, chains = 1, iter = 500, writeDir = "out")

rstan::plot(myFit, pars = c("xBeta11", "xBeta21"))

print(rstan::summary(myFit)[[1]][1:18, ], digits = 2)
