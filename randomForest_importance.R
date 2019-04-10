### Random Forest and Var importance ###

library(ranger)

# Simulation
x = matrix(runif(5*1e4, -1,1), 1e4, 5)
w = runif(5,0,8)
y = x %*% w + rnorm(1e4,0,0.5)

summary(lm(y~x))

rf = ranger::ranger(y~., data = data.frame(y = y, x),num.trees = 500,
                    importance = 'impurity_corrected', mtry = 2)
vars = ranger::importance(rf)
plot(log(vars)~log(w))



rf = ranger::ranger(y~., data = data.frame(y = y, x),num.trees = 500,
                    importance = 'permutation', mtry = 2)
(vars = ranger::importance(rf))
plot(log(vars)~log(w))



# with weights = 0 and error on x
x = matrix(runif(8*1e4, -1,1), 1e4, 8)
w = c(runif(5,0,8), 0,0,0)
y = x %*% w + rnorm(1e4,0,0.5)
x = apply(x, 2, function(d) d+rnorm(1e4,0,0.5))

m1 = lm(y~x)
summary(m1)
coefficients(m1)

rf = ranger::ranger(y~., data = data.frame(y = y, x),num.trees = 500,
                    importance = 'impurity_corrected', mtry = 3,min.node.size = 20)
(vars = ranger::importance(rf))
ranger::importance_pvalues(rf, 'altmann', formula = formula(y~.), data = data.frame(y=y,x))
plot(log(vars)~log(w))


p = predict(rf, data = data.frame(y = y, x))
plot(p$predictions, y)
points(predict(m1,data = data.frame(y = y, x) ), y, col = "red")

# colinearity
x = matrix(runif(2*1e4, -1,1), 1e4, 2)
w = c(3,1,0,0)
x = cbind(x, x[,1] * 0.3 + rnorm(1e4,0,0.5), x[,2] * - 0.1 + rnorm(1e4,0,0.5))
y = x %*% w + rnorm(1e4,0,0.5)
x = apply(x, 2, function(d) d+rnorm(1e4,0,0.5))
m1 = lm(y~x)
summary(m1)
coefficients(m1)

rf = ranger::ranger(y~., data = data.frame(y = y, x),num.trees = 500,
                    importance = 'impurity_corrected', mtry = 3,min.node.size = 20)
(vars = ranger::importance(rf))
plot(log(vars+10)~log(w+0.01))

ranger::importance_pvalues(rf, 'altmann', formula = formula(y~.), 
                           data = data.frame(y=y,x)[1:100,])

