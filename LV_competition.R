##########################################################################################
# Lotka-Volterra model for competition system  ---------------------------------------
##########################################################################################

# Notes -------------------------------------------------------------------
# The competition variant has logistic growth! (exponential in predator-prey model)
# There are solutions for one equilibrium, multiple equilibria, no equilibria


# Library -----------------------------------------------------------------
library(deSolve)


# Two-species model -------------------------------------------------------------------
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
         a11 = 0.2,
         a21 = 0.2,
         a22 = 0.2,
         a12 = 0.1)
state_init <- c(0.1, 1)
time = 1:1000

Sim_2 <- ode(y = state_init, times = time, func = calcComp2, parms = par2)
matplot(Sim_2[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Phragmites australis", "Valeriana dioica"), lty = c(1,2), col = c(1,2), box.lwd = 0)


# Multi-species matrix model -------------------------------------------------------------------

calcCompM <- function(t,
                      m, # A vector of species states.
                      par){
  r <- par[[1]] # Length n vector of growth rates.
  A <- par[[2]] # Full n x n matrix of competition factors.
  dm <- r * m * (1 - (A %*% m)) # This is logistic as well.
  return(list(c(dm)))
}

# Multi-species simulation -------------------------------------------------------------------
n_species <- 9
time <- 1:2000

## Generate some random parameters.
r <- runif(n_species)*2 # Vector of growth rates.

alpha <- .01 # The mean competition.
A <- matrix(rnorm(n_species^2, alpha, alpha/10), nrow = n_species) # A full matrix of mutual competition factors.

par <- list(r, A) # Parameters list, including a matrix of alpha values.

m0 <- runif(n_species)/(n_species*alpha) # Initial state matrix.

Sim_m <- ode(m0, t, calcCompM, par)
matplot(t, Sim_m[,-1], type="l", ylab="N") # log='y'


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

