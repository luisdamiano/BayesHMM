## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  echo     = TRUE, 
  eval     = TRUE, 
  cache    = TRUE,
  collapse = TRUE,
  comment  = "#>"
)

## ----inference_table, echo = FALSE, eval = TRUE--------------------------
df <- data.frame(
  name = c("Filtering", 
           "Smoothing", 
           "Fixed lag smoothing", 
           "State prediction", 
           "Observation prediction", 
           "MAP Estimation", 
           "Log likelihood"),
  hidden = c("$p(z_t | \\mat{y}_{1:t})$",
             "$p(z_t | \\mat{y}_{1:T})$",
             "$p(z_{t - \\ell} | \\mat{y}_{1:t})$, $\\ell \\ge 1$",
             "$p(z_{t+h} | \\mat{y}_{1:t})$, $h\\ge 1$",
             "$p(y_{t+h} | \\mat{y}_{1:t})$, $h\\ge 1$",
             "$\\argmax_{\\mat{z}_{1:T}} p(\\mat{z}_{1:T} | \\mat{y}_{1:T})$",
             "$p(\\mat{y}_{1:T})$"),
  availability = c("$t$ (online)",
                   "$T$ (offline)",
                   "$t + \\ell$ (lagged)",
                   "$t$",
                   "$t$",
                   "$T$",
                   "$T$"),
  algorithm = c("Forward",
                "Forward-backward",
                "Forward-backward",
                "",
                "",
                "Viterbi decoding",
                "Forward"),
  complexity = c("$O(K^2T)$ \\ $O(KT)\\dagger$",
                 "$O(K^2T)$ \\ $O(KT)\\dagger$",
                 "$O(K^2T)$ \\ $O(KT)\\dagger$",
                 "",
                 "",
                 "$O(K^2T)$",
                 "$O(K^2T)$ \\ $O(KT)\\dagger$")
)

df.col <- c("Name",
            "Hidden Quantity",
            "Availability at",
            "Algorithm",
            "Complexity")

knitr::kable(df, format = "pandoc", col.names = df.col, 
             caption = "Summary of the hidden quantities and their corresponding inference algorithm. $\\dagger$ Time complexity is reduced to $O(KT)$ for inference on a left-to-right (upper triangular) transition matrix.")

## ---- include = FALSE, eval = FALSE, echo = FALSE------------------------
#  Constants
#  R		    Observation dimension
#  K		    Number of hidden states
#  M       Number of covariates for the observation model
#  P       Number of covariates for the transition model
#  Q       Number of covariates for the initial model
#  
#  Covariates
#  x_t		  Time-varying covariates for the observation model
#  u_t		  Time-varying covariates for the transition model
#  v  		  Covariates for the initial model
#  
#  Known-stochastic quantities
#  y_t		  Observation vector
#  
#  Model parameters
#  *_kr		Ex. mu_11, sigma_11 (k, r suffixes are optional)
#  A		    Transition model parameters (if no covariates)
#  pi		  Initial distribution parameters (if no covariates)
#  xBeta	  Regression parameters for the observation model
#  uBeta	  Regression parameters for the transition model
#  vBeta	  Regression parameters for the initial model
#  
#  Estimated quantities
#  z_t 	  Hidden state
#  alpha_t	Filtered probability
#  gamma_t	Smoothed probability
#  zstar		Viterbi
#  
#  (Prior/Posterior) predictive quantities
#  yPred		Sample of observations drawn from the  predictive density
#  zPred		Sample of latent path drawn from the  predictive density
#  
#  Note: time suffix t is optional.

## ---- echo = FALSE-------------------------------------------------------
library(BayesHMM)

## ----eval = FALSE--------------------------------------------------------
#  devtools::install_github("luisdamiano/BayesHMM", ref = "master")
#  library(BayesHMM)

## ---- eval = FALSE-------------------------------------------------------
#  hmm(
#    K = 3, R = 2,
#    observation = {...}
#    initial     = {...}
#    transition  = {...}
#    name = "Model name"
#  )

## ------------------------------------------------------------------------
mySpec <- hmm(
  K = 3, R = 1,
  observation =
    Gaussian(mu = -10, sigma = 10) +
    Gaussian(mu =   0, sigma = 10) +
    Gaussian(mu =  10, sigma = 10),
  initial     = Dirichlet(alpha = Default()),
  transition  = Dirichlet(alpha = Default()),
  name = "Univariate Gaussian"
)

## ------------------------------------------------------------------------
mySpec <- hmm(
  K = 2, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = Default(),
  transition  = TransitionSoftmax(
    uBeta = Gaussian(0, 5)
  ),
  name = "TVHMM Softmax Univariate Gaussian"
)

## ------------------------------------------------------------------------
mySpec <- hmm(
  K = 2, R = 1,
  observation = Gaussian(
    mu    = Gaussian(0, 10),
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL))
  ),
  initial     = InitialSoftmax(
    vBeta = Gaussian(0, 5)
  ),
  transition  = Dirichlet(alpha = Default()),
  name = "Univariate Gaussian with covariates for initial model"
)

## ---- results = "hide"---------------------------------------------------
  mySpec <- hmm(
    K = 3, R = 2,
    observation =
      Gaussian(mu = Gaussian(mu = -10, sigma = 1), sigma = 1) +
      Gaussian(mu = Gaussian(mu =   0, sigma = 1), sigma = 1) +
      Gaussian(mu = Gaussian(mu =  10, sigma = 1), sigma = 1),
    initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
    transition  =
      Dirichlet(alpha = c(1.0, 0.2, 0.2)) +
      Dirichlet(alpha = c(0.2, 1.0, 0.2)) +
      Dirichlet(alpha = c(0.2, 0.2, 1.0)),
    name = "Dummy Model"
  )

  val <- validate_calibration(mySpec, N = 2, T = 100, iter = 500, seed = 9000)

## ------------------------------------------------------------------------
knitr::kable(
  head(val$chains), 
  digits = 2, caption = "Chains diagnostics"
)

knitr::kable(
  head(val$parameters), 
  digits = 2, caption = "Parameters diagnostics"
)

plot(
  val$parameters$Rhat, 
  type = "h", ylim = c(0.9, 1.15),
  ylab = expression(hat(R)),
  xlab = "Validation run (n)"
)
abline(h = 1.1)

## ---- results = "hide"---------------------------------------------------
mySpec <- hmm(
  K = 3, R = 1,
  observation =
    Gaussian(mu = Gaussian(mu = -10, sigma = 1), sigma = 1) +
    Gaussian(mu = Gaussian(mu =   0, sigma = 1), sigma = 1) +
    Gaussian(mu = Gaussian(mu =  10, sigma = 1), sigma = 1),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Gaussian"
)

set.seed(9000)
y <- as.matrix(
  c(rnorm(100, -10, 1), rnorm(100, 0, 1), rnorm(100, 10, 1))
)
y <- sample(y)

myModel <- compile(mySpec)
myAll   <- optimizing(mySpec, myModel, y = y, nRun = 10, keep = "all", nCores = 4)
myBest <- extract_best(myAll)

## ------------------------------------------------------------------------
print(
  round(extract_grid(myAll, pars = "mu"), 2)
)

## ------------------------------------------------------------------------
mySpec <- hmm(
  K = 3, R = 1,
  observation =
    Gaussian(mu = Gaussian(mu = -10, sigma = 1), sigma = 1) +
    Gaussian(mu = Gaussian(mu =   0, sigma = 1), sigma = 1) +
    Gaussian(mu = Gaussian(mu =  10, sigma = 1), sigma = 1),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  name = "Univariate Gaussian"
)

set.seed(9000)
y <- as.matrix(
  c(rnorm(100, -10, 1), rnorm(100, 0, 1), rnorm(100, 10, 1))
)
y <- sample(y)

myFit <- fit(mySpec, y = y)

# 3 parameters, 1,000 iterations, 4 chains
print(
  str(extract_obs_parameters(myFit))
)

# reduce within-chain draws to one quantity (median)
print(
  extract_obs_parameters(myFit, reduce = median)
)

# reduce within-chain draws to two quantities (quantiles)
print(
  extract_obs_parameters(
    myFit, reduce = posterior_intervals(c(0.1, 0.9))
  )
)

# combine quantities into a matrix
print(
  extract_obs_parameters(
      myFit, 
      reduce  = median,
      combine = rbind
  )
)

## ------------------------------------------------------------------------
print(
  str(classify_alpha(myFit))
)

print(
  str(classify_gamma(myFit))
)

print(
  str(classify_zstar(myFit))
)

## ---- results = "hide"---------------------------------------------------
mySpec <- hmm(
  K = 3, R = 2,
  observation =
    Gaussian(mu = Gaussian(mu = -10, sigma = 1), sigma = 1) +
    Gaussian(mu = Gaussian(mu =   0, sigma = 1), sigma = 1) +
    Gaussian(mu = Gaussian(mu =  10, sigma = 1), sigma = 1),
  initial     = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Dirichlet(alpha = c(1.0, 0.2, 0.2)) +
    Dirichlet(alpha = c(0.2, 1.0, 0.2)) +
    Dirichlet(alpha = c(0.2, 0.2, 1.0)),
  name = "Univariate Gaussian Dummy Model"
)

mySim <- sim(mySpec, T = 300, seed = 9000, iter = 500, chains = 1)
ySim  <- extract_ypred(mySim)[[1]][1, , ]
colnames(ySim) <- c("Weight", "Height")

myFit <- fit(mySpec, y = ySim, seed = 9000, iter = 500, chains = 1)

## ------------------------------------------------------------------------
plot_series(myFit, legend.cex = 0.8)

plot_series(myFit, xlab = "Time steps", features = c("yColoredLine"))

plot_series(
  myFit, stateProbability =  "smoothed", 
  features = c("stateShade", "bottomColoredMarks")
)

plot_state_probability(myFit, main = "Title", xlab = "Time")

plot_state_probability(myFit, features = c("stateShade"), main = "Title", xlab = "Time")

# plot_state_probability(myFit, features = c("bottomColoredMarks"), main = "Title", xlab = "Time")

plot_state_probability(myFit, features = c("probabilityColoredDots"), main = "Title", xlab = "Time")

plot_state_probability(myFit, features = c("probabilityColoredLine"), main = "Title", xlab = "Time")

# plot_state_probability(myFit, stateProbability = "filtered", features = c("bottomColoredMarks", "probabilityFan"), stateProbabilityInterval = c(0.05, 0.95), main = "Title", xlab = "Time")

plot_ppredictive(myFit, type = c("density", "cumulative", "summary"), fun = median)

plot_ppredictive(myFit, type = c("density", "boxplot"), fun = median, subset = 1:10)

plot_ppredictive(
  myFit, 
  type = c("density", "boxplot", "scatter"), 
  fun = median, fun1 = mean, fun2 = median, 
  subset = 1:40
)

plot_ppredictive(myFit, type = c("density", "cumulative", "hist"), fun = median)

plot_ppredictive(myFit, type = c("density", "cumulative", "ks"))

## ---- results = "hide"---------------------------------------------------
mySpec <- mixture(
  K = 3, R = 1,
  observation = Student(
    mu    = Default(),
    sigma = Gaussian(0,  10, bounds = list(0, NULL)),
    nu    = Gaussian(0, 100, bounds = list(0, NULL))
  ),
  initial     = Dirichlet(alpha = Default()),
  name = "Univariate Student t Mixture"
)

set.seed(9000)
y <- as.matrix(
  c(rnorm(50, 5, 1), rnorm(300, 0, 1), rnorm(100, -5, 1))
)

myFit <- fit(mySpec, y = y, chains = 1, iter = 500)

## ------------------------------------------------------------------------
print_fit(myFit)

## ---- warning = FALSE----------------------------------------------------
# OBSERVATION MODEL -------------------------------------------------------

K = 3
R = 2

# Case 1. A different multivariate density for each state
#   Input: K multivariate densities
#   Behaviour: Nothing

exCase1  <- hmm(
  K = K, R = R,
  observation =
    MVGaussianCor(
      mu    = Gaussian(mu = 0, sigma = 1),
      L     = LKJCor(eta = 2)
    ) +
    MVGaussianCor(
      mu    = Gaussian(mu = 0, sigma = 10),
      L     = LKJCor(eta = 3)
    ) +
    MVGaussianCor(
      mu    = Gaussian(mu = 0, sigma = 100),
      L     = LKJCor(eta = 4)
    ),
  initial     = Default(),
  transition  = Default(),
  name = "A different multivariate density for each each state"
)

# Case 2. Same multivariate density for every state
#   Input: One multivariate density
#   Behaviour: Repeat input K times

exCase2  <- hmm(
  K = K, R = R,
  observation =
    MVGaussianCor(
      mu    = Gaussian(mu = 0, sigma = 100),
      L     = LKJCor(eta = 2)
    ),
  initial     = Default(),
  transition  = Default(),
  name = "Same multivariate density for every state"
)

# Case 3. Same univariate density for every state and every output variable
#   Input: One univariate density
#   Behaviour: Repeat input K %nested% R times

exCase3  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial     = Default(),
  transition  = Default(),
  name = "Same univariate density for every state and every output variable"
)

# Case 4. Same R univariate densities for every state
#   Input: R univariate densities
#   Behaviour: Repeat input K times

exCase4  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial     = Default(),
  transition  = Default(),
  name = "Same R univariate densities for every state"
)

# Case 5. Same univariate density for every output variable
#   Input: K univariate densities
#   Behaviour: Repeat input R times

exCase5  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial     = Default(),
  transition  = Default(),
  name = "Same univariate density for every output variable"
)

# Case 6. Different univariate densities for every pair of state and output variable
#   Input: K %nested% R univariate densities
#   Behaviour: None

exCase6  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial     = Default(),
  transition  = Default(),
  name = "Different univariate densities for every pair of state and output variable"
)

# UNIVARIATE Observation model --------------------------------------------

K = 3
R = 1

# Case 7. A different univariate density for each each state
#   Input: K univariate densities
#   Behaviour: Nothing

exCase7  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial     = Default(),
  transition  = Default(),
  name = "A different univariate density for each each state"
)

# Case 8. Same univariate density for every state
#   Input: One univariate density
#   Behaviour: Repeat input K times

exCase8  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial     = Default(),
  transition  = Default(),
  name = "Same multivariate density for every state"
)

# INITIAL MODEL -----------------------------------------------------------
K = 3
R = 2

# Case 9. Same univariate density for every initial state
#   Input: One univariate density
#   Behaviour: Repeat input K times
exCase9  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial =
    Beta(
      alpha = Gaussian(0, 1),
      beta  = Gaussian(1, 10)
    ),
  transition  = Default(),
  name = "Same univariate density for every initial state"
)

# Case 10. One multivariate density for the whole initial vector
#   Input: One multivariate density
#   Behaviour: Nothing
exCase10  <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial =
    Dirichlet(
      alpha = Default()
    ),
  transition  = Default(),
  name = "One multivariate density for the whole initial vector"
)

# Case 11. A different univariate density for each initial state
#   Input: K univariate densities
#   Behaviour: Nothing
exCase11 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  transition  = Default(),
  name = "A different univariate density for each initial state"
)

# Case 12. A link
#   Input: One link density
#   Behaviour: Nothing
exCase12 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial =
    InitialSoftmax(
      vBeta = Default()
    ),
  transition  = Default(),
  name = "TV Initial distribution"
)

# TRANSITION MODEL --------------------------------------------------------
K = 3
R = 2

# Case 13. Same univariate density for every transition
#   Input: One univariate density
#   Behaviour: Repeat input KxK times
exCase13 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Gaussian(mu = 0, sigma = 1),
  name = "Same univariate density for every transition"
)

# Case 14. Same multivariate density for every transition row
#   Input: One multivariate density
#   Behaviour: Repeat input K times

exCase14 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Dirichlet(
      alpha = c(0.5, 0.5, 0.7)
    ),
  name = "Same multivariate density for every transition row"
)

# Case 15. A different univariate density for each element of the transition row
#   Input: K univariate densities
#   Behaviour: Repeat input K times

exCase15 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Beta(alpha = 0.1, beta = 0.1) +
    Beta(alpha = 0.5, beta = 0.5) +
    Beta(alpha = 0.9, beta = 0.9),
  name = "A different univariate density for each element of the transition row"
)

# Case 16. A different multivariate density for each transition row
#   Input: K multivariate densities
#   Behaviour: nothing

exCase16 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Dirichlet(alpha = c(0.1, 0.1, 0.1)) +
    Dirichlet(alpha = c(0.5, 0.5, 0.5)) +
    Dirichlet(alpha = c(0.9, 0.9, 0.9)),
  name = "A different multivariate density for each transition row"
)

# Case 17. Different univariate densities for each element of the transition matrix
#   Input: KxK univariate densities
#   Behaviour: None

exCase17 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    Beta(alpha = 0.1, beta = 0.1) +
    Beta(alpha = 0.2, beta = 0.2) +
    Beta(alpha = 0.3, beta = 0.3) +
    Beta(alpha = 0.4, beta = 0.4) +
    Beta(alpha = 0.5, beta = 0.5) +
    Beta(alpha = 0.6, beta = 0.6) +
    Beta(alpha = 0.7, beta = 0.7) +
    Beta(alpha = 0.8, beta = 0.8) +
    Beta(alpha = 0.9, beta = 0.9),
  name = "Different univariate densities for each element of the transition matrix"
)

# Case 18. A link
#   Input: One link density
#   Behaviour: Nothing

exCase18 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)),
  transition  =
    TransitionSoftmax(
      uBeta = Gaussian(mu = 0, sigma = 1)
    ),
  name = "Different univariate densities for each element of the transition matrix"
)

# A FULLY COMPLEX MODEL ---------------------------------------------------

# Case 19. A link in both the transition and initial distribution.
#   Input: A link in both the transition and initial distribution.
#   Behaviour: Nothing
exCase19 <- hmm(
  K = K, R = R,
  observation =
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ) +
    Gaussian(
      mu    = Gaussian(0, 10),
      sigma = Gaussian(0, 10, bounds = list(0, NULL))
    ),
  initial =
    InitialSoftmax(
      vBeta = Gaussian(mu = 0, sigma = 1)
    ),
  transition  =
    TransitionSoftmax(
      uBeta = Gaussian(mu = 0, sigma = 1)
    ),
  name = "Fully Complex Model"
)

