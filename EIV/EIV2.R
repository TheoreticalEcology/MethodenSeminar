## simulate data
X = runif(1000, -1, 1)
Y = 2*X + rnorm(1000, sd = 1.0)

m1 = lm(Y~X)
summary(m1)

##
X_obs = X + rnorm(1000, sd = 0.5)
m2 = lm(Y~X_obs)
summary(m)


plot(X, Y, col = "red")
points(X_obs, Y, col = "blue")
abline(m1, col = "red")
abline(m2, col = "blue")

library(brms)
br = brm(Y~me(X_obs, 0.5), data = data.frame(Y=Y, X_obs = X_obs))
summary(br)
plot(br)

brms::stancode(br)


### Our own model
library(rstan)
stan_model = rstan::stan("EIV/my_EIV.stan", data= list(y = Y, x_obs = X_obs, sigma_x = 0.5, N = 1000), 
                         iter = 10000, chains = 2)
summary(stan_model)

plot(stan_model)


parameters = rstan::extract(stan_model)
plot(density(parameters$beta))
median( parameters$beta )
median( parameters$sigma_x )

bayesplot::mcmc_areas(
  stan_model, 
  pars = c("beta", "sigma_x"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean")











