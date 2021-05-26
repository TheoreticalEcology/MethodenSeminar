//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y_obs;
  vector[N] prior_y;
  vector[N] x;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real beta;
  real beta0;
  real prior_mu;
  real<lower=0> sigma_y;
  real y_true[N];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  sigma_y ~ cauchy(0, 3);
  prior_mu ~ normal(0, 3);
  beta0 ~ normal(0, 5);
  beta ~ normal(0, 5);
  
  y_true ~ normal(prior_mu, 5);
  
  y_obs ~ normal(y_true, prior_y);
  
  y_true ~ normal( x*beta + beta0, sigma_y);
  
}

