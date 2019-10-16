x = runif(100, -3,3)

y = x*3 + 2 + rnorm(100,0,0.5)

#################################

data = data.frame(x, y)

summary(lm(y~x, data = data))

likelihood = function(params){
  
  y_hat = params[1]*data$x + params[2]
  
  
  if(params[3] > 0) ll = dnorm(y, mean = y_hat, sd = params[3], log = TRUE)
  else ll = -Inf
  
  return(-sum(ll))
  
  
}



opt = optim(par = c(1,1,1),fn = likelihood)
opt

likelihood(c(3,2,0.5))








x = runif(100, -3,3)
#Normal
y = x*3 + 2 + rnorm(100,0,0.5)
#Poisson
y = rpois(100, lambda = exp(x*3 + 2)) 


#################################

data = data.frame(x, y)

summary(glm(y~x, data = data, family = poisson))

likelihood = function(params){
  
  y_hat = params[1]*data$x + params[2]
  
  ll = dpois(y, lambda = exp(y_hat), log = T)
  
  
  return(-sum(ll))
  
  
}
(opt = optim(c(10,10), likelihood, method = "SANN")$par)

summary(lm(log(y+0.001)~x,data = data))

#############
# Bootstrap

data

plot(y ~ x, data = data)

# 

bs <- function() {
  idx <- sample.int(n = 100, size = 100, replace = TRUE)
  bs1 <- data[idx, ]
  fit <- glm(y ~ x, data = data[idx,], family = poisson)
  fit$coefficients
}

plot(y ~ x, data = data)
plot(y ~ x, data = bs1)

bs()

bs_res <- matrix(nrow= 100, ncol = 2)

for (i in 1:1000) {
  bs_res[i,] <- bs()
}

hist(bs_res[,1])
hist(bs_res[,2])

sd(bs_res[,1])
sd(bs_res[,2])

rbinom(100,size = 10,prob = 1/10)










