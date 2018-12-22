test_density.txt_1 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Bernoulli(theta = 0.5), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_2 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Bernoulli(theta = 0.5), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_3 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Bernoulli(theta = Beta(alpha = 0.5, 
    beta = 0.5, bounds = list(0, 1))), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_4 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Beta(1, 1), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_5 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Beta(alpha = Default(bounds = list(0, 
    NULL)), beta = Default(bounds = list(0, NULL))), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_6 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Binomial(theta = 0.5, N = 100), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_7 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Binomial(theta = Beta(alpha = 0.5, 
    beta = 0.5, bounds = list(0, 1)), N = 100), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_8 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Categorical(theta = c(0.25, 0.25, 
    0.25, 0.25), N = 4), initial = Dirichlet(alpha = c(0.5, 0.5, 
    0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_9 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Categorical(theta = Dirichlet(alpha = c(0.5, 
    0.5, 0.5, 0.5)), N = 4), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_10 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Cauchy(mu = 0, sigma = 1), initial = Dirichlet(alpha = c(0.1, 
    0.5, 1)), transition = Dirichlet(alpha = c(0.1, 0.5, 1)))") } 

test_density.txt_11 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Cauchy(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 2, bounds = list(0, 
        NULL))), initial = Dirichlet(alpha = c(0.1, 0.5, 1)), 
    transition = Dirichlet(alpha = c(0.1, 0.5, 1)))") } 

test_density.txt_12 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Gaussian(mu = 0, sigma = 1), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_13 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, 
        NULL))), initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), 
    transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_14 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussian(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(100, 0, 0, 100), 2, 2)), sigma = InverseWishart(nu = 5, 
    sigma = matrix(c(1, 0, 0, 1), 2, 2))), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_15 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussianCholeskyCor(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(1, 0, 0, 1), 2, 2)), L = CholeskyLKJCor(1)), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_16 <- function() { no_error_in_spec("hmm(K = 3, R = 4, observation = Multinomial(theta = c(0.25, 0.25, 
    0.25, 0.25), N = 10), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_17 <- function() { no_error_in_spec("hmm(K = 3, R = 4, observation = Multinomial(theta = Default(), 
    N = 10), initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)))") } 

test_density.txt_18 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussian(mu = c(0, 0), sigma = matrix(c(1, 
    0, 0, 1), 2, 2)), initial = Dirichlet(alpha = c(0.5, 0.5, 
    0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_19 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussian(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(100, 0, 0, 100), 2, 2)), sigma = InverseWishart(nu = 5, 
    sigma = matrix(c(1, 0, 0, 1), 2, 2))), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_20 <- function() { no_error_in_spec("hmm(K = 3, R = 2, MVGaussianCholeskyCor(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(1, 0, 0, 1), 2, 2)), L = CholeskyLKJCor(1)), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_21 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussianCholeskyCor(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(100, 0, 0, 100), 2, 2)), L = CholeskyLKJCor(1)), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_22 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVStudent(mu = c(0, 0), sigma = matrix(c(1, 
    0, 0, 1), 2, 2), nu = 2), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_23 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVStudent(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(100, 0, 0, 100), 2, 2)), sigma = InverseWishart(nu = 5, 
    sigma = matrix(c(1, 0, 0, 1), 2, 2)), nu = Gamma(2, 0.1)), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_24 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = NegativeBinomial(alpha = 1, beta = 2), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_25 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = NegativeBinomial(alpha = Default(bounds = list(0, 
    NULL)), beta = Default(bounds = list(0, NULL))), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_26 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = NegativeBinomialLocation(mu = 1, 
    phi = 2), initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), 
    transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_27 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = NegativeBinomialLocation(mu = Gamma(1, 
    1), phi = Gamma(1, 1)), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_28 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Poisson(5), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_29 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Poisson(lambda = Gamma(1, 1)), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_30 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Student(mu = 0, sigma = 1, nu = 1), 
    initial = Dirichlet(alpha = c(0.1, 0.5, 1)), transition = Dirichlet(alpha = c(0.1, 
        0.5, 1)))") } 

test_density.txt_31 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Student(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 2, bounds = list(0, 
        NULL)), nu = Cauchy(mu = 0, sigma = 10, bounds = list(0, 
        NULL))), initial = Dirichlet(alpha = c(0.1, 0.5, 1)), 
    transition = Dirichlet(alpha = c(0.1, 0.5, 1)))") } 

