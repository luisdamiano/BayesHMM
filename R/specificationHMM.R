#' Specify a Hidden Markov Model.
#'
#' @inherit specify
#' @family models
#' @examples
#' mySpec   <- hmm(
#'   K = 2, R = 1,
#'   observation = Gaussian(
#'     mu    = Gaussian(0, 10),
#'     sigma = Student(
#'       mu = 0, sigma = 10, nu = 1, bounds = list(0, NULL)
#'     )
#'   ),
#'   initial     = Dirichlet(alpha = c(1, 1)),
#'   transition  = Dirichlet(alpha = c(1, 1)),
#'   name = "Univariate Gaussian Hidden Markov Model"
#' )
hmm <- function(K, R, observation = NULL, initial = NULL,
                transition = NULL, name = "") {
  x <- specify(K, R, observation, initial, transition, name)
  class(x) <- append(class(x), "HMMSpecification", 0)
  x
}

# Undocumented internal methods -------------------------------------------

block_functions.HMMSpecification <- function(spec) {
  if (is.TVTransition(spec)) {
    "
    #include tvhmm-forward.stan
    #include tvhmm-forwardBackward.stan
    #include tvhmm-mappath.stan
    #include tvhmm-zpredictive.stan
    "
  } else {
    "
    #include hmm-forward.stan
    #include hmm-forwardBackward.stan
    #include hmm-mappath.stan
    #include hmm-zpredictive.stan
    "
  }
}

block_data.HMMSpecification <- function(spec) {
  strAll <-
    "
    int<lower = 1> K; // number of hidden states
    int<lower = 1> R; // dimension of the observation vector
    "

  strInitial <-
    if (is.TVInitial(spec)) {
      "
      int<lower = 1> Q;     // number of initial model predictors
      vector[Q] v;          // initial model predictors
      "
    } else {
      ""
    }

  strTransition <-
    if (is.TVTransition(spec)) {
      "
      int<lower = 1> P;     // number of transition model predictors
      matrix[T, P] u;       // transition model predictors
      "
    } else {
      ""
    }

  c(strAll, strInitial, strTransition)
}

block_parameters.HMMSpecification <- function(spec) {
  strInitial <-
    if (is.TVInitial(spec)) {
      ""
    } else {
      "
      simplex[K] pi;                    // initial state probabilities
      "
    }

  strTransition <-
    if (is.TVTransition(spec)) {
      ""
    } else if (is.FixedTransition(spec)) {
      ""
    } else {
      "
      simplex[K] A[K];                  // transition probabilities
                                        // A[i][j] = p(z_t = j | z_{t-1} = i)
      "
    }

  c(strInitial, strTransition)
}

block_tparameters.HMMSpecification <- function(spec) {
  strAll <-
    "
    vector[T] logalpha[K];
    vector[K] logpi;
    "

  strInitial <-
    if (is.TVInitial(spec)) {
      strAll <- paste0(strAll, "\n", "vector[K] pi;")

      "
      #include initialLink.stan
      "
    } else {
      "logpi = log(pi);"
    }

  strTransition <-
    if (is.TVTransition(spec)) {
      strAll <- paste0(
        strAll,
        "\n",
        "vector[K] A[T, K];",
        "\n",
        "\t\tvector[K] logA[T, K];\t\t\t\t\t\t // transition logA[t, from, to]"
      )

      "
        for (t in 1:T) {
          for (i in 1:K) { // i = previous (t-1)
            #include transitionLink.stan
          }
          logA[t] = log(A[t]);
        }
      "
    } else if (is.FixedTransition(spec)) {
      strAll <- paste0(
        strAll,
        "\n",
        "simplex[K] A[K];\t\t\t\t\t\t\t\t\t\t// transition probabilities\n",
        "\t\t\t\t\t\t\t\t\t\t// A[i][j] = p(z_t = j | z_{t-1} = i)\n",
        "\n",
        "\t\tvector[K] logA[K];\t\t\t\t\t\t // transition logA[from, to]"
      )

      "
      for (i in 1:K) { // i = previous (t-1)
        #include transitionLink.stan
      }
      logA = log(A);
      "
    } else {
      strAll <- paste0(
        strAll,
        "\n",
        "\t\tvector[K] logA[K];\t\t\t\t\t\t // transition logA[from, to]"
      )
      "
      logA = log(A);
      "
    }

  c(strAll, strInitial, strTransition)
}

block_generated.HMMSpecification <- function(spec) {
  if (is.TVTransition(spec)) {
    "
    vector[T] alpha[K];
    vector[T] gamma[K];
    int<lower=1, upper=K> zstar[T];

    for (t in 1:T)
    alpha[, t] = to_array_1d(softmax(to_vector(logalpha[, t])));

    gamma = forwardbackward(K, T, logpi, logA, loglike, alpha);
    zstar = MAPpath(K, T, logpi, logA, loglike);
    "
  } else {
    "
    vector[T] alpha[K];
    vector[T] gamma[K];
    int<lower=1, upper=K> zstar[T];

    for (t in 1:T)
      alpha[, t] = to_array_1d(softmax(to_vector(logalpha[, t])));

    gamma = forwardbackward(K, T, logpi, logA, loglike, alpha);
    zstar = MAPpath(K, T, logpi, logA, loglike);
    "
  }
}

chunk_calculate_target.HMMSpecification <- function(spec) {
  "
  logalpha = forward(K, T, logpi, logA, loglike);
  "
}

chunk_increase_target.HMMSpecification <- function(spec) {
  "
  target += log_sum_exp(logalpha[, T]);
  "
}

chunk_zpredictive.HMMSpecification <- function(spec) {
  "
  zpred = zpredictive_rng(K, T, pi, A);
  "
}
