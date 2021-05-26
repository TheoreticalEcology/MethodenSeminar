### Simulate data ###

p1 = p2 = NULL
for(i in 1:5000) {
  x = runif(100, -1, 1)
  Y = 0.5*x + rnorm(100, sd = 0.5) # error on y
  
  sds = runif(100, 0.5, 3.0)
  Y_obs = Y + rnorm(100, sd = sds)
  
  sum = summary(lm(Y~x))
  p1[i] = sum$coefficients[2,4]
  
  sum = summary(lm(Y_obs~x))
  p2[i] = sum$coefficients[2,4]
  
  sum = summary(glmmTMB(Y_obs~x, data=data.frame(Y_obs = Y_obs, x=x), weights = sds))
}

mean(p1<0.05)
mean(p2<0.05)



stan_model = rstan::stan("EIV/EIV_model2.stan", data = list(y_obs = Y_obs, x = x, N = 100, prior_y = sds), 
                         iter = 10000, chains = 2L )
plot(stan_model, pars = "beta", ci_level = 0.95)

parameters = rstan::extract(stan_model)
plot(density(parameters$sigma_y))

b = brm(Y_obs~x, data =data.frame(Y_obs = Y_obs, x=x))
summary(b)

data = data.frame(Y = Y, X_e = X_e, X = x)

posterior_interval(
  stan_model,
  prob = 0.9,
  type = "central",
  pars = NULL,
  regex_pars = NULL)


library(brms)
model = brm(Y~me(X_e, 0.3), data = data)
summary(model)


library(rstan)
stan_model = rstan::stan("EIV/simple.stan", data = list(y = Y, x_obs = X_e, N = 100), iter = 20000, chains = 2L )
summary(stan_model)


parameters = rstan::extract(stan_model)
plot(density(parameters$beta))

