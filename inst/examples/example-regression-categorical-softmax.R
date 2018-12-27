mySpec <- hmm(
  K = 2, R = 1,
  observation = RegCategoricalSoftmax(
    xBeta = MVGaussian(mu = c(0, 0, 0), sigma = ImproperUniform()),
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

myFit <- fit(mySpec, y = y, x = x, chains = 1, iter = 500, seed = 9000)

plot_series(myFit)

print_fit(myFit)
