test_density.txt_1 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Bernoulli(theta = 0.5), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_2 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Bernoulli(theta = Beta(alpha = 0.5, 
    beta = 0.5, bounds = list(0, 1))), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_3 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Bernoulli(theta = Beta(alpha = 0.5, 
    beta = 0.5, ordered = TRUE, bounds = list(0, 1))), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_4 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Beta(1, 1), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_5 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Beta(alpha = ImproperUniform(bounds = list(0, 
    NULL)), beta = ImproperUniform(bounds = list(0, NULL))), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_6 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Beta(alpha = ImproperUniform(bounds = list(0, 
    NULL)), beta = ImproperUniform(bounds = list(0, NULL))), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_7 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Binomial(theta = 0.5, N = 100), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_8 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Binomial(theta = Beta(alpha = 0.5, 
    beta = 0.5, bounds = list(0, 1)), N = 100), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_9 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Binomial(theta = Beta(alpha = 0.5, 
    beta = 0.5, ordered = TRUE, bounds = list(0, 1)), N = 100), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_10 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Categorical(theta = c(0.25, 0.25, 
    0.25, 0.25), N = 4), initial = Dirichlet(alpha = c(0.5, 0.5, 
    0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_11 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Categorical(theta = Dirichlet(alpha = c(0.5, 
    0.5, 0.5, 0.5)), N = 4), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_12 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Cauchy(mu = 0, sigma = 1), initial = Dirichlet(alpha = c(0.1, 
    0.5, 1)), transition = Dirichlet(alpha = c(0.1, 0.5, 1)))") } 

test_density.txt_13 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Cauchy(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 2, bounds = list(0, 
        NULL))), initial = Dirichlet(alpha = c(0.1, 0.5, 1)), 
    transition = Dirichlet(alpha = c(0.1, 0.5, 1)))") } 

test_density.txt_14 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Cauchy(mu = Gaussian(0, 10, ordered = TRUE), 
    sigma = Student(mu = 0, sigma = 10, nu = 2, bounds = list(0, 
        NULL))), initial = Dirichlet(alpha = c(0.1, 0.5, 1)), 
    transition = Dirichlet(alpha = c(0.1, 0.5, 1)))") } 

test_density.txt_15 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussianCholeskyCor(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(1, 0, 0, 1), 2, 2)), L = CholeskyLKJCor(1)), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_16 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Gaussian(mu = 0, sigma = 1), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_17 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, 
        NULL))), initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), 
    transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_18 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Gaussian(mu = Gaussian(0, 10, 
    ordered = TRUE), sigma = Student(mu = 0, sigma = 10, nu = 1, 
    bounds = list(0, NULL))), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_19 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, 
        NULL))), initial = InitialFixed(pi = c(0.2, 0.2, 0.6)), 
    transition = Dirichlet(alpha = c(1, 1, 1)))") } 

test_density.txt_20 <- function() { no_error_in_spec("hmm(K = 2, R = 1, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, 
        NULL))), initial = InitialSoftmax(vBeta = Gaussian(0, 
    10), Q = 2), transition = Dirichlet(alpha = c(0.5, 0.5)))") } 

test_density.txt_21 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussian(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(100, 0, 0, 100), 2, 2)), sigma = InverseWishart(nu = 5, 
    sigma = matrix(c(1, 0, 0, 1), 2, 2))), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_22 <- function() { no_error_in_spec("hmm(K = 3, R = 4, observation = Multinomial(theta = c(0.25, 0.25, 
    0.25, 0.25), N = 10), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_23 <- function() { no_error_in_spec("hmm(K = 3, R = 4, observation = Multinomial(theta = ImproperUniform(), 
    N = 10), initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)))") } 

test_density.txt_24 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussian(mu = c(0, 0), sigma = matrix(c(1, 
    0, 0, 1), 2, 2)), initial = Dirichlet(alpha = c(0.5, 0.5, 
    0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_25 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussian(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(100, 0, 0, 100), 2, 2)), sigma = InverseWishart(nu = 5, 
    sigma = matrix(c(1, 0, 0, 1), 2, 2))), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_26 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussian(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(100, 0, 0, 100), 2, 2), ordered = TRUE), 
    sigma = InverseWishart(nu = 5, sigma = matrix(c(1, 0, 0, 
        1), 2, 2))), initial = Dirichlet(alpha = c(0.5, 0.5, 
    0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_27 <- function() { no_error_in_spec("hmm(K = 3, R = 2, MVGaussianCholeskyCor(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(1, 0, 0, 1), 2, 2)), L = CholeskyLKJCor(1)), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_28 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVGaussianCholeskyCor(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(100, 0, 0, 100), 2, 2)), L = CholeskyLKJCor(1)), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_29 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVStudent(mu = c(0, 0), sigma = matrix(c(1, 
    0, 0, 1), 2, 2), nu = 2), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_30 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVStudent(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(100, 0, 0, 100), 2, 2)), sigma = InverseWishart(nu = 5, 
    sigma = matrix(c(1, 0, 0, 1), 2, 2)), nu = GammaDensity(2, 
    0.1)), initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)))") } 

test_density.txt_31 <- function() { no_error_in_spec("hmm(K = 3, R = 2, observation = MVStudent(mu = MVGaussian(mu = c(0, 
    0), sigma = matrix(c(100, 0, 0, 100), 2, 2), ordered = TRUE), 
    sigma = InverseWishart(nu = 5, sigma = matrix(c(1, 0, 0, 
        1), 2, 2)), nu = GammaDensity(2, 0.1)), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_32 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = NegativeBinomial(alpha = 1, beta = 2), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_33 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = NegativeBinomial(alpha = ImproperUniform(bounds = list(0, 
    NULL)), beta = ImproperUniform(bounds = list(0, NULL))), 
    initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
        0.5, 0.5)))") } 

test_density.txt_34 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = NegativeBinomialLocation(mu = 1, 
    phi = 2), initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), 
    transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_35 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = NegativeBinomialLocation(mu = GammaDensity(1, 
    1), phi = GammaDensity(1, 1)), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_36 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = NegativeBinomialLocation(mu = GammaDensity(1, 
    1, ordered = TRUE), phi = GammaDensity(1, 1)), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_37 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Poisson(5), initial = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_38 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Poisson(lambda = GammaDensity(1, 
    1)), initial = Dirichlet(alpha = c(0.5, 0.5, 0.5)), transition = Dirichlet(alpha = c(0.5, 
    0.5, 0.5)))") } 

test_density.txt_39 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Poisson(lambda = GammaDensity(1, 
    1, ordered = TRUE)), initial = Dirichlet(alpha = c(0.5, 0.5, 
    0.5)), transition = Dirichlet(alpha = c(0.5, 0.5, 0.5)))") } 

test_density.txt_40 <- function() { no_error_in_spec("hmm(K = 2, R = 1, observation = RegBernoulliLogit(xBeta = Gaussian(mu = 0, 
    sigma = 10), M = 3), initial = Dirichlet(alpha = c(0.5, 0.5)), 
    transition = Dirichlet(alpha = c(0.5, 0.5)))") } 

test_density.txt_41 <- function() { no_error_in_spec("hmm(K = 2, R = 1, observation = RegBinomialLogit(xBeta = Gaussian(mu = 0, 
    sigma = 10), M = 3, N = 10), initial = Dirichlet(alpha = c(0.5, 
    0.5)), transition = Dirichlet(alpha = c(0.5, 0.5)))") } 

test_density.txt_42 <- function() { no_error_in_spec("hmm(K = 2, R = 1, observation = RegBinomialProbit(xBeta = Gaussian(mu = 0, 
    sigma = 10), M = 3, N = 10), initial = Dirichlet(alpha = c(0.5, 
    0.5)), transition = Dirichlet(alpha = c(0.5, 0.5)))") } 

test_density.txt_43 <- function() { no_error_in_spec("hmm(K = 2, R = 1, observation = RegGaussian(xBeta = Gaussian(mu = 0, 
    sigma = 10), sigma = Student(mu = 0, sigma = 10, nu = 1, 
    bounds = list(0, NULL)), M = 3), initial = Dirichlet(alpha = c(0.5, 
    0.5)), transition = Dirichlet(alpha = c(0.5, 0.5)))") } 

test_density.txt_44 <- function() { no_error_in_spec("hmm(K = 2, R = 1, observation = RegGaussian(xBeta = Gaussian(mu = 0, 
    sigma = 10), sigma = Student(mu = 0, sigma = 10, nu = 1, 
    bounds = list(0, NULL), ordered = TRUE), M = 3), initial = Dirichlet(alpha = c(0.5, 
    0.5)), transition = Dirichlet(alpha = c(0.5, 0.5)))") } 

test_density.txt_45 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Student(mu = 0, sigma = 1, nu = 1), 
    initial = Dirichlet(alpha = c(0.1, 0.5, 1)), transition = Dirichlet(alpha = c(0.1, 
        0.5, 1)))") } 

test_density.txt_46 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Student(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 2, bounds = list(0, 
        NULL)), nu = Cauchy(mu = 0, sigma = 10, bounds = list(0, 
        NULL))), initial = Dirichlet(alpha = c(0.1, 0.5, 1)), 
    transition = Dirichlet(alpha = c(0.1, 0.5, 1)))") } 

test_density.txt_47 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Student(mu = Gaussian(0, 10, 
    ordered = TRUE), sigma = Student(mu = 0, sigma = 10, nu = 2, 
    bounds = list(0, NULL)), nu = Cauchy(mu = 0, sigma = 10, 
    bounds = list(0, NULL))), initial = Dirichlet(alpha = c(0.1, 
    0.5, 1)), transition = Dirichlet(alpha = c(0.1, 0.5, 1)))") } 

test_density.txt_48 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Student(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 2, bounds = list(0, 
        NULL), ordered = TRUE), nu = Cauchy(mu = 0, sigma = 10, 
        bounds = list(0, NULL))), initial = Dirichlet(alpha = c(0.1, 
    0.5, 1)), transition = Dirichlet(alpha = c(0.1, 0.5, 1)))") } 

test_density.txt_49 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Student(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 2, bounds = list(0, 
        NULL)), nu = Cauchy(mu = 0, sigma = 10, bounds = list(0, 
        NULL), ordered = TRUE)), initial = Dirichlet(alpha = c(0.1, 
    0.5, 1)), transition = Dirichlet(alpha = c(0.1, 0.5, 1)))") } 

test_density.txt_50 <- function() { no_error_in_spec("hmm(K = 3, R = 1, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, 
        NULL))), initial = Dirichlet(alpha = c(1, 1, 1)), transition = TransitionFixed(A = matrix(c(0.5, 
    0.2, 0.3), ncol = 3, nrow = 3, byrow = TRUE)))") } 

test_density.txt_51 <- function() { no_error_in_spec("hmm(K = 2, R = 1, observation = Gaussian(mu = Gaussian(0, 10), 
    sigma = Student(mu = 0, sigma = 10, nu = 1, bounds = list(0, 
        NULL))), initial = Dirichlet(alpha = c(0.5, 0.5)), transition = TransitionSoftmax(uBeta = Gaussian(0, 
    10), P = 2))") } 

