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
  vector[N] x;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real beta;
  real beta0;
  real prior_mu;
  real<lower=0> prior_sigma;
  real<lower=0> sigma_x;
  real<lower=0> sigma_y;
  real x_true[N];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  sigma_x ~ cauchy(0, 5);
  sigma_y ~ cauchy(0, 5);
  prior_sigma ~ cauchy(0, 5);
  prior_mu ~ normal(0, 1);
  beta0 ~ normal(0, 5);
  beta ~ normal(0, 5);
  
  x_true ~ normal(prior_mu, prior_sigma);
  
  x ~ normal(x_true, sigma_x);
  
  for(i in 1:N) {
    y[i] ~ normal(x_true[i]*beta + beta0, sigma_y);
  }
  
}

