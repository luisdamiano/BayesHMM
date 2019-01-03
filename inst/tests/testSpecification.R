test_specification.txt_1 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Gaussian(mu = -5, sigma = 1) + 
    Gaussian(mu = 0, sigma = 1) + Gaussian(mu = 5, sigma = 1), 
    initial = Dirichlet(alpha = c(1, 1, 1)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_specification.txt_2 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussianCholeskyCor(mu = Gaussian(mu = 0, 
    sigma = 1), L = CholeskyLKJCor(eta = 2)) + MVGaussianCholeskyCor(mu = Gaussian(mu = 0, 
    sigma = 10), L = CholeskyLKJCor(eta = 3)) + MVGaussianCholeskyCor(mu = Gaussian(mu = 0, 
    sigma = 100), L = CholeskyLKJCor(eta = 4)), initial = ImproperUniform(), 
    transition = ImproperUniform())") } 

test_specification.txt_3 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussianCholeskyCor(mu = Gaussian(mu = 0, 
    sigma = 100), L = CholeskyLKJCor(eta = 2)), initial = ImproperUniform(), 
    transition = ImproperUniform())") } 

test_specification.txt_4 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Gaussian(0, 10, bounds = list(0, NULL))), initial = ImproperUniform(), 
    transition = ImproperUniform())") } 

test_specification.txt_5 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Gaussian(0, 10, bounds = list(0, NULL))) + Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))), initial = ImproperUniform(), 
    transition = ImproperUniform())") } 

test_specification.txt_6 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Gaussian(0, 10, bounds = list(0, NULL))) + Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))) + Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))), initial = ImproperUniform(), 
    transition = ImproperUniform())") } 

test_specification.txt_7 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Gaussian(0, 10, bounds = list(0, NULL))) + Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))) + Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))) + Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))) + Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))) + Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))), initial = ImproperUniform(), 
    transition = ImproperUniform())") } 

test_specification.txt_8 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Gaussian(0, 10, bounds = list(0, NULL))) + Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))) + Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))), initial = ImproperUniform(), 
    transition = ImproperUniform())") } 

test_specification.txt_9 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Gaussian(0, 10, bounds = list(0, NULL))), initial = ImproperUniform(), 
    transition = ImproperUniform())") } 

test_specification.txt_10 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Gaussian(0, 10, bounds = list(0, NULL))), initial = Beta(alpha = Gaussian(0, 
    1), beta = Gaussian(1, 10)), transition = ImproperUniform())") } 

test_specification.txt_11 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Gaussian(0, 10, bounds = list(0, NULL))), initial = Dirichlet(alpha = ImproperUniform()), 
    transition = ImproperUniform())") } 

test_specification.txt_12 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Gaussian(0, 10, bounds = list(0, NULL))), initial = Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))) + Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))) + Gaussian(mu = Gaussian(0, 
    10), sigma = Gaussian(0, 10, bounds = list(0, NULL))), transition = ImproperUniform())") } 

test_specification.txt_13 <- function() { no_error_in_spec("hmm(K = 2, R = 1, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, 
        NULL))), initial = Dirichlet(alpha = c(1, 1)), transition = TransitionSoftmax(uBeta = Gaussian(0, 
    10), P = 2))") } 

test_specification.txt_14 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Gaussian(0, 10, bounds = list(0, NULL))), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.7)))") } 

test_specification.txt_15 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Gaussian(0, 10, bounds = list(0, NULL))), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.1, 0.1, 0.1)) + 
    Dirichlet(alpha = c(1, 1, 1)) + Dirichlet(alpha = c(0.9, 
    0.9, 0.9)))") } 

