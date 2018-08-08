loglike[1][t] = normal_lpdf(y[t] | mu11, sigma11);
loglike[1][t] = normal_lpdf(y[t] | mu12, sigma12);
loglike[2][t] = normal_lpdf(y[t] | mu21, sigma21);
loglike[2][t] = normal_lpdf(y[t] | mu22, sigma22);
loglike[3][t] = normal_lpdf(y[t] | mu31, sigma31);
loglike[3][t] = normal_lpdf(y[t] | mu32, sigma32);
