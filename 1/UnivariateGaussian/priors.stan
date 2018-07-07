A[1, ] ~ dirichlet([1, 0.2, 0.2]');
A[2, ] ~ dirichlet([0.2, 1, 0.2]');
A[3, ] ~ dirichlet([0.2, 0.2, 1]');
mu11 ~ normal(-10, 1) ;
mu21 ~ normal(0, 1) ;
mu31 ~ normal(10, 1) ;
