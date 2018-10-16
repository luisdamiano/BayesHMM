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

