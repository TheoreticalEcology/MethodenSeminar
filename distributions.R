

## Probability distributions are related and can be derived from each other.
## See, e.g., here https://www.johndcook.com/blog/distribution_chart/,
## and here http://www.math.wm.edu/~leemis/2008amstat.pdf.


##########################################################################
# Discrete distributions ------------------------------------------------
##########################################################################


## Bernouilli --------------------------------------------------------------

bernoulli <- function(n, p) {
  rbinom(n, size = 1, p)
}

hist(bernoulli(100, 0.9))



## Binomial ----------------------------------------------------------------
binomial <- function(n, m, p) {
  replicate(n, sum(bernoulli(m, p)))
}

hist(binomial(1000, 3, 0.3))
hist(rbinom(1000, 3, 0.3))


## Poisson ----------------------------------------------------------------
poisson <- function(n, lambda) {
   ## Poisson experiment is the number of successful bernoullis in a sample, where the average number of successes is lambda
  ## lambda == size_binom*p
  size_binom <- 100
  p <- lambda/size_binom
  replicate(n, rbinom(1000, size_binom, p))
}

hist(poisson(1000, 1.5))
hist(rpois(1000, 3))


##  Negative binomial -------------------------------------------------------
## Generative approach
## https://stats.stackexchange.com/questions/176034/negative-binomial-distribution-vs-binomial-distribution

nbinomial_one <-function(size, p) {
  trial <- 0
  successes <- 0
  while (successes < size) {
    trial <- trial+1
    successes <- successes + bernoulli(1, p)
  }
  return(trial - successes) # return no of failures!
}

nbinomial <- function(n, size, prob) {
  replicate(n, nbinomial_one(size, prob))
}

hist(nbinomial(1000, 12, 0.2))
hist(rnbinom(1000, 12, 0.2))


# Normal from binomial ---------------------------------------------------------

# mean = n*p
# p = mean/n
# sd = n*p*(1-p)

normal <- function(n, mean) {
  size <- 1000000000
  p <- mean/size
  rbinom(n, size, p)
}

hist(normal(1000, 100))



##########################################################################
# Continuous distributions ------------------------------------------------
##########################################################################

# CLT ---------------------------------------------------------

normal <- function(n = 100,
                   m = 20) {
  rowSums(replicate(m, runif(n, -1, 1)))
}

hist(normal())
