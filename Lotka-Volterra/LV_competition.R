##########################################################################################
# Lotka-Volterra model for competition system  ---------------------------------------
##########################################################################################

# Notes -------------------------------------------------------------------
# The competition variant has logistic growth! (exponential in predator-prey model)
# There are solutions for one equilibrium, multiple equilibria, no equilibria


# Library -----------------------------------------------------------------
library(deSolve)


# One species logistic -------------------------------------------------------------------
calcLogistic <- function(time, state, par)
{
  with(as.list(par), {
    dN <- r * state * (1 - state/K) ## 1/K = a11
    list(dN)
  })
}

# Two-species simulation --------------------------------------------------------------
par <- c(r = 0.1,
         K = 100)
state_init <- c(N = 10)
time = 1:100

Sim_logistic <- ode(y = state_init, times = time, func = calcLogistic, parms = par)
matplot(Sim_logistic[,-1], type = "l", xlab = "time", ylab = "population")

# Two-species model -------------------------------------------------------------------

## states N, M of two species
## dn/dt ==  r_n N * (1 - a_nn N - a_nm M)
## increment of rate r_n*N is at max for both very small intraspecific and interspecific competition
## a12 is the competition of M on N
## as states N or M grow bigger, r_n*N approaches 0

calcComp2 <- function (time, state, par)
{
  with(as.list(par), {
    dn <- r_n * state[1] * (1 - a11 * state[1] - a12 * state[2])
    dm <- r_m * state[2] * (1 - a22 * state[2] - a21 * state[1])
    list(c(dn, dm))
  })
}

# Two-species simulation --------------------------------------------------------------
par2 <- c(r_n = 0.05,
         r_m = 0.05,
         a11 = 0.2, ## interaction coefficients
         a21 = 0.3,
         a22 = 0.2,
         a12 = 0.1)
state_init <- c(0.1, 1)
time = 1:1000

Sim_2 <- ode(y = state_init, times = time, func = calcComp2, parms = par2)
matplot(Sim_2[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Phragmites australis", "Valeriana dioica"), lty = c(1,2), col = c(1,2), box.lwd = 0)


# Multi-species matrix model -------------------------------------------------------------------

## For one species the model equation is:
## dn/dt ==  r_n N * (1 - (a_nn N + a_nm M))

## Same for multiple species:
## dm/dt ==  r m * (1 - (A %*% m))
## first term (r m): here, r, m are just vectorized
## second term (1 - (A %*% m)): for a two species model equivalent to
## c(A_11 m1 + A_12 m2, A_21 m1 + A_22 m2)


calcCompM <- function(t,
                      m, # A vector of species states.
                      par){
  r <- par[[1]] # Length n vector of growth rates.
  A <- par[[2]] # Full n x n matrix of interaction coefficients
  dm <- r * m * (1 - (A %*% m)) # This is logistic as well.
  return(list(c(dm)))
}

# Multi-species simulation -------------------------------------------------------------------
n_species <- 9
time <- 1:500

## Generate some random parameters.
r <- runif(n_species)*2 # Vector of growth rates.

alpha <- .01 # The mean interaction
A <- matrix(rnorm(n_species^2, alpha, alpha/10), nrow = n_species) # A full matrix of mutual competition factors.

par <- list(r, A) # Parameters list, including a matrix of alpha values.

m0 <- runif(n_species)/(n_species*alpha) # Initial state matrix.

Sim_m <- ode(m0, time, calcCompM, par)
matplot(Sim_m[,-1], type="l", ylab="N") # log='y'



# Data --------------------------------------------------------------------
## Longitudinal community data is available at
# library(dave) [Package for WILDI text book]
# data(lveg)
# data(ltim)
# ?lveg
# ?ltim


# gauseR --------------------------------------------------------------------
## Just as a note that this exists, including data:
# library(gauseR)






