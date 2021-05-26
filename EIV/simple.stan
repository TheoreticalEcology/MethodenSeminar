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
  real beta;
  real beta0;
  real mu_prior;
  vector[N] x;
  real<lower=0> sigma_x;
  real<lower=0> sigma_y;
  real<lower=0> mu_sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  x ~ normal(mu_prior, mu_sigma);
  sigma_x ~ normal(0, 3);
  sigma_y ~ normal(0, 3);
  x_obs ~ normal(x, sigma_x);
  y ~ normal(x*beta+beta0, sigma_y);
}

