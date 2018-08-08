real mu11;
real<lower = 0> sigma11;
real mu12;
real<lower = 0> sigma12;
real mu21;
real<lower = 0> sigma21;
real mu22;
real<lower = 0> sigma22;
real mu31;
real<lower = 0> sigma31;
real mu32;
real<lower = 0> sigma32;

        matrix[K, Q] vBeta;   // initial model regressors
                                // vBeta[state, Q regressors]
        

        matrix[K, P] uBeta[K];        // transition model regressors
                                        // uBeta[to, from, p regressors]
        
