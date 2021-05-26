
# What prediction error do we estimate?
# ErrXY: the error of the model that was fit on our actual training set 
# Err: the average error of the fitting algorithm run on the same-sized 
#      datasets drawn from the underlying distribution P

devtools::install_github("stephenbates19/nestedcv")
library(nestedcv)
library(glmnet)

## sample data

n = 90    # no. of obs.
p = 1000   ## no. of features
k = 4     ## no. of non-0 coefficient

set.seed(123)
X = matrix(rnorm(n=n*p), nrow = n)
st = 1
beta = st * c(rep(1,k), rep(0, p-k))

## sample resp : sparse logistic regression model
prbst = 1 / (1+ exp(-st * X %*% beta))
Y = (runif(n) < prbst) * 1

## fit glm model
fit = cv.glmnet(X, Y, family = "binomial")
names(fit)
plot(fit)

lambda.fit = fit$lambda
optim.lambda = match(fit$lambda.1se, lambda.fit)
lambda = lambda.fit[1:optim.lambda]

## take a holdout set
a_holdout = 5000
X_holdout = matrix(rnorm(a_holdout * p), nrow = a_holdout)   
j_holdout = 1/(1+ exp(-X_holdout) %*% beta)
Y_holdout = (runif(a_holdout) < j_holdout) * 1

by.error1 = (rowSums(X_holdout[, 1:k]) < 0) * (Y_holdout == 1) + (rowSums(X_holdout[, 1:k]) > 0) * (Y_holdout == 0)
error.mean = mean(by.error1)
print(error.mean)

##################################################################################

#### subroutines

misclass_loss <- function(y1, y2, funcs_params = NA) {
  y1 != y2
} 
fitter_glmnet_logistic <- function(X, Y, idx = NA, funcs_params = NA) {
  if(sum(is.na(idx)) > 0) {idx <- 1:nrow(X)}
  fit <- glmnet(X[idx, ], Y[idx], family = "binomial", lambda = funcs_params$lambda) #assumes lambda is in global env
  
  fit
}
predictor_glmnet_logistic <- function(fit, X_new, funcs_params = NA) {
  beta_hat <- fit$beta[, funcs_params$optim.lambda] #assumes optim.lambda is in global env
  a0_hat <- fit$a0[funcs_params$optim.lambda]
  preds <- (X_new %*% beta_hat + a0_hat > 0)
  
  preds
} 
logistic_lasso_funs <- list(fitter = fitter_glmnet_logistic,
                            predictor = predictor_glmnet_logistic,
                            loss = misclass_loss,
                            name = "logistic_lasso")

#################

n_folds = 5
nested_cv_reps = 300  ## random splits

## nested CV
t1 = Sys.time()
set.seed(144)
nested_cv_dd = nested_cv(X, Y, logistic_lasso_funs, n_folds = n_folds,
                         reps = nested_cv_reps, funcs_params = list("lambdas" = lambda, "optim.lambda" = optim.lambda), 
                         verbose = T)

t2 = Sys.time()
print(t2-t1)

## standard CV
naive_cv_dd = naive_cv(X,Y, logistic_lasso_funs, n_folds = n_folds,
                       funcs_params = list("lambdas" = lambda, "optim.lambda" = optim.lambda))


#### plot the evaluation results

plot(nested_cv_dd$running_sd_infl,
     xlab= "number of random splits", ylab = "estimate of sd_infl")

tr.error = mean(misclass_loss(predict(fit, X_holdout, s="lambda.1se", type = "response") > 0.5, Y_holdout))

print(paste0("lower_ci: ", naive_cv_dd$ci_lo, "||",tr.error, "|| higher ci: ", naive_cv_dd$ci_hi))
print(paste0("lower_ci: ", nested_cv_dd$ci_lo, "||",tr.error, "|| higher ci: ", nested_cv_dd$ci_hi))

