mySpec <- hmm(
  K = 3, R = 1,
  observation =
    Gaussian(mu = -5, sigma = 1)
    + Gaussian(mu = 0, sigma = 1)
    + Gaussian(mu = 5, sigma = 1),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Gaussian Generative"
)

myFit <- sim(mySpec, chains = 1, iter = 500, seed = 9000)
# Alternatively, you can set the number of generated quantities T
# run(mySpec, T = 200, chains = 1, iter = 500)

plot(extract_ypred(myFit, permuted = FALSE)[1, 1, ], type = "l")

print_all(myFit)
