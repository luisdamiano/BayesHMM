# Full Bayesian Inference for Hidden Markov Models

We create an R Package to run full Bayesian inference on Hidden Markov Models (HMM) using the probabilistic programming language Stan. By providing an intuitive, expressive yet flexible input interface, we enable non-technical users to carry out research using the Bayesian workflow. We provide the user with an expressive interface to mix and match a wide array of options for the observation and latent models, including ample choices of densities, priors, and link functions whenever covariates are present. The software enables users to fit HMM with time-homogeneous transitions as well as time-varying transition probabilities. Priors can be set for every model parameter. Implemented inference algorithms include forward (filtering), forward-backwards (smoothing), Viterbi (most likely hidden path), prior predictive sampling, and posterior predictive sampling. Graphs, tables and other convenience methods for convergence diagnosis, goodness of fit, and data analysis are provided.

[Google Summer of Code 2018](https://summerofcode.withgoogle.com/projects/#4681157036212224)

### Install

```
devtools::install_github("luisdamiano/BayesHMM")
```

