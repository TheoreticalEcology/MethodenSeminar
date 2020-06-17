# https://mc-stan.org/users/documentation/case-studies/lotka-volterra-predator-prey.html#data-lynx-and-hare-pelts-in-canada
# https://www.biorxiv.org/content/10.1101/2020.03.16.993642v2.full.pdf
library(rstan)
library(deSolve)


lynx_hare_df <-
  read.csv("Lotka-Volterra/hudson-bay-lynx-hare.csv",
           comment.char="#")

N <- length(lynx_hare_df$Year) - 1
ts <- 1:N
y_init <- c(lynx_hare_df$Hare[1], lynx_hare_df$Lynx[1])
y <- as.matrix(lynx_hare_df[2:(N + 1), 2:3])
y <- cbind(y[ , 2], y[ , 1]); # hare, lynx order
lynx_hare_data <- list(N = N, ts = ts, y_init = y_init, y = y)

model <- stan_model("Lotka-Volterra/model.stan")

fit <- sampling(model, data = lynx_hare_data, seed = 123)


print(fit, pars=c("theta", "sigma", "z_init"),
      probs=c(0.1, 0.5, 0.9), digits = 3)







GLV <- function(t, x, parameters){
  with(as.list(c(x, parameters)), {
    x[x < 10^-8] <- 0 # prevent numerical problems
    dxdt <- x * (r + A %*% x)
    list(dxdt)
  })
}
# function to plot output
plot_ODE_output <- function(out){
  out <- as.data.frame(out)
  colnames(out) <- c("time", paste("sp", 1:(ncol(out) -1), sep = "_"))
  out <- as_tibble(out) %>% gather(species, density, -time)
  pl <- ggplot(data = out) + 
    aes(x = time, y = density, colour = species) + 
    geom_line()
  show(pl)
  return(out)
}
# general function to integrate GLV
integrate_GLV <- function(r, A, x0, maxtime = 100, steptime = 0.5){
  times <- seq(0, maxtime, by = steptime)
  parameters <- list(r = r, A = A)
  # solve numerically
  out <- ode(y = x0, times = times, 
             func = GLV, parms = parameters, 
             method = "ode45")
  # plot and make into tidy form
  out <- plot_ODE_output(out)
  return(out)
}
set.seed(3) # for reproducibility
r_3 <- rep(1, 3)
A_3 <- -matrix(c(10, 6, 12, 
                 14, 10, 2, 
                 8, 18, 10), 3, 3, byrow = TRUE)
# check the existence of feasible equilibrium
print(solve(A_3, -r_3)) # feasible
x0_3 <- 0.1 * runif(3)
res_3 <- integrate_GLV(r_3, A_3, x0_3, maxtime = 250)


library(BayesianTools)
integrate_GLV <- function(r, A, x0, maxtime = 100, steptime = 0.5){
  times <- seq(0, maxtime, by = steptime)
  parameters <- list(r = r, A = A)
  # solve numerically
  out <- ode(y = x0, times = times, 
             func = GLV, parms = parameters, 
             method = "ode45")
  return(out)
}

pars = c(as.vector(A_3), r_3, x0_3, 0.3)


density = function(par){
  d1 = sum(dunif(par[1:9], -40,-1, log = TRUE))
  d2 = sum(dunif(par[10:15], 0, 3, log = TRUE))
  d3 = dunif(par[16],0,3,log = TRUE)
  return(d1+d2+d3)
}

sampler = function(n=1){
  ff = function() {
    r1 = (runif(9, -40,-1))
    r2 = (runif(6, 0, 3))
    r3 = runif(1,0,3.0)
    return(c(r1,r2,r3))
  }
  return(t(sapply(1:n, function(i) ff())))
}

ll = function(par) {
  A_pred = matrix(par[1:9],3,3)
  x0_pred = par[10:12]
  r_pred = par[13:15]
  pp=integrate_GLV(r_pred, A_pred, x0_pred, maxtime = 50)
  ll = sum(dlnorm(res_3[,2:4],log(pp[,2:4]),sdlog = par[16], log = TRUE))
  if(is.na(ll)) ll = -Inf
  return(ll)
}

(ss=sampler()[1,])
ll(ss)
opt = optim(ss, function(par) -ll(par))
matrix(opt$par[1:9],3,3)

prior = createPrior(density = density, sampler = sampler)
bs = createBayesianSetup(likelihood = ll, prior = prior, parallel = TRUE)
settings = list(iterations = 30000)
model = runMCMC(bs, settings = settings)


matrix(BayesianTools::MAP(model)$parametersMAP[1:9],3,3)
# devtools::install_github("https://github.com/adamtclark/gauseR/")
library(gauseR)
gause_out<-gause_wrapper(time=res_3[,1], species=data.frame(res_3[,2:4]))
matrix(gause_out$parameter_intervals$mu[7:15],3,3,byrow = TRUE)