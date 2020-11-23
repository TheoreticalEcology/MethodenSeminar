library(tensorflow)
# install_tensorflow()
library(tfprobability)
# install_tfprobability()
library(cmdstanr)
# install_cmdstan()

# Simulate data from three-level hierarchical process ---------------------------------
## model structure with two nested groups: groups1/groups2/observations
n_groups1 <- 15
n_groupswithin1 <- 10 # groups2
n_groups2 <- n_groups1 * n_groupswithin1
n_obswithin2 <- 5
n <- n_groups1 * n_groupswithin1 * n_obswithin2

groups1 <- rep(1:n_groups1, each = n_groupswithin1*n_obswithin2)
groups2 <- rep(1:n_groups2, each = n_obswithin2)
id <- 1:n

G <- cbind(groups1, groups2, id)
  
x1 <- runif(n_groups1, -1, 1)
x3 <- runif(n, -1, 1)

## level 0
m0 <- 3.3 # overall mean

## level 1, dependent on x1
m1 <- rnorm(n_groups1, mean = m0, sd = 7.7)
b1 <- 2
m1 <- m1 + b1*x1
m1 <- rep(m1, each = n_groupswithin1) # length == n_groups2

## level 2, independent of a predictor, independent across group
m2 <- rnorm(n_groups2, mean = m1, sd = 8.8)
m2 <- rep(m2, each = n_obswithin2) # length == n_groups2

## level 3, dependent on x3
b3 <- -2
y_hat <- m2 + b3 * x3
## observation error
y <- rnorm(n, mean = y_hat, sd = 4)

D <- data.frame(G, y, x1 = x1[groups1], x3)
boxplot(y ~ groups1:groups2, data = D[1:175,], col = groups1)

# Stan fit ----------------------------------------------------------------
standata <- list(y = D$y,
                 x3 = x3,
                 x1 = x1,
                 
                 N_groups1 = length(unique(D$groups1)),
                 N_groups2 = length(unique(D$groups2)),
                 N = nrow(D),
                 
                 lookup2in3 = D$groups2,
                 lookup1in2 = unique(D[c('groups2', 'groups1')])$groups1,
                 
                 hypersigma = 0.5
                 )

## Compilation
stanmodel <- cmdstan_model('Hierarchical/Hierarchical.stan')

## Optimization
stanoptim <- stanmodel$optimize(data = standata)
stanoptim$summary() %>% View()

## NUTS sampling
standraw <- stanmodel$sample(data = standata,
                             chains = 3,
                             parallel_chains = getOption("mc.cores", 3))
standraw$summary()
## TMB with Laplace sampling


# mle fit -------------------------------------------------------------
fit <- lm(y ~ x3, data = D)
summary(fit)
fit <- lme4::lmer(y ~ x3 + (1 | groups1/groups2), data = D)
summary(fit)


