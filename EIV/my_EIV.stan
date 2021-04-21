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
  vector[N] y;
  vector[N] x_obs;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real beta0;
  real beta;
  real prior_mu;
  real<lower=0> sigma_y;
  real<lower=0> sigma_x;
  real<lower=0> prior_sigma;
  vector[N] x_true;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // priors
  x_true ~ normal(prior_mu, prior_sigma);
  sigma_y ~ normal(0, 5);
  sigma_x ~ normal(0, 5);
  prior_sigma ~ normal(0, 5);
  x_obs ~ normal(x_true, sigma_x);
  
  // likelihood
  y ~ normal( x_true*beta + beta0, sigma_y);
}

