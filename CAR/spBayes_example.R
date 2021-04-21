install.packages("spBayes")
library(spBayes)

# function to make sure inputs are compatible
rmvn <-function(n, mu=0, V =matrix(1)){
  p <-length(mu)
  if(any(is.na(match(dim(V),p))))
    stop("Dimension problem!")
  D <-chol(V)
  t(matrix(rnorm(n*p), ncol=p)%*%D+ rep(mu,rep(n,p)))
  }

set.seed(1)


### the underlying model is the following 

# y(s) = x(s)*beta + w(s) + epsilon(s)
# where s denotes the locations 
# Correlation(s,t) = sigma^2 rho(s,t,phi)
# s.t. rho(s,t,phi) = exp(-phi ||s-t||) and sigma^2 = var(w(s))


# n is number of observations
n <- 100

# coords are observation coordinates for spLM()
coords <-cbind(runif(n,0,1),runif(n,0,1))

# X is matrix of regressors
X <-as.matrix(cbind(1,rnorm(n)))

# B is slope 
B <-as.matrix(c(1,5))

# p is number of parameters
p <-length(B)


sigma.sq <- 2 #spatial process
tau.sq <- 0.1 # random noise variance 
phi <- 3/0.5 # correlation 

# D is distance matrix for coordinates

D <-as.matrix(dist(coords))

# R is correlation function
R <-exp(-phi*D)

# w is a spatial process
w <-rmvn(1,rep(0,n), sigma.sq*R)

# y is a vector of spatially referenced dependent variables
y <-rnorm(n, X%*%B+w,sqrt(tau.sq))

# number of MCMC iterations, used in spLM()
n.samples <- 2000

# starting values for parameters in spLM()
starting <-list("phi"=3/0.5, "sigma.sq"=50, "tau.sq"=1)

# variances for the Metropolis sampler in spLM()
tuning <-list("phi"=0.1, "sigma.sq"=0.1, "tau.sq"=0.1)

# priors for parameters in spLM(): betas are multivariate Normal
priors.1 <-list("beta.Norm"=list(rep(0,p),diag(1000,p)),"phi.Unif"=c(3/1, 3/0.1), "sigma.sq.IG"=c(2, 2),"tau.sq.IG"=c(2, 0.1))

# priors for parameters in spLM(): betas are flat
priors.2 <-list("beta.Flat", "phi.Unif"=c(3/1, 3/0.1),
                "sigma.sq.IG"=c(2, 2), "tau.sq.IG"=c(2, 0.1))

# function for spatial dependence structure in spLM()
cov.model <- "exponential"

# interval for seeing progress of the sampler in spLM()
n.report <- 500

# model with first set of priors

m.1 <-spLM(y~X-1, coords=coords, starting=starting,tuning=tuning, priors=priors.1,
           cov.model=cov.model,n.samples=n.samples, n.report=n.report)

# model with second set of priors
m.2 <-spLM(y~X-1, coords=coords, starting=starting,tuning=tuning, priors=priors.2, 
           cov.model=cov.model,n.samples=n.samples, n.report=n.report)

par(mfrow=c(2,2))
ts.plot(m.1$p.theta.samples[,1],main="sigma sq",ylab="",xlim=c(100,nrow(m.1$p.theta.samples)),ylim=c(0,4))
ts.plot(m.1$p.theta.samples[,2],main="tau sq",ylab="",xlim=c(100,nrow(m.1$p.theta.samples)),ylim=c(0,1))
ts.plot(m.1$p.theta.samples[,3],main="phi",ylab="",xlim=c(50,nrow(m.1$p.theta.samples)))


ts.plot(m.2$p.theta.samples[,1],main="sigma sq",ylab="",xlim=c(100,nrow(m.1$p.theta.samples)),ylim=c(0,4))
ts.plot(m.2$p.theta.samples[,2],main="tau sq",ylab="",xlim=c(100,nrow(m.1$p.theta.samples)),ylim=c(0,1))
ts.plot(m.2$p.theta.samples[,3],main="phi",ylab="",xlim=c(50,nrow(m.1$p.theta.samples)))

# recover beta and spatial random effects
burn.in <- 0.5*n.samples
m.1 <-spRecover(m.1, start=burn.in, verbose=FALSE)
m.2 <-spRecover(m.2, start=burn.in, verbose=FALSE)


##########################
###Predictive process model
###########################

# to predict we use the same function, but just give knots, to which will be predicted 

m.1 <-spLM(y~X-1, coords=coords, knots=c(6,6,0.1), starting=starting,tuning=tuning,
           priors=priors.1, cov.model=cov.model,n.samples=n.samples, n.report=n.report)

m.2 <-spLM(y~X-1, coords=coords, knots=c(6,6,0.1), starting=starting,tuning=tuning,
           priors=priors.2, cov.model=cov.model,n.samples=n.samples, n.report=n.report)


#######################
### Multivariate model 
######################

# n is number of observations
n <- 25
q <- 2 #number of outcomes at each location
nltr <- q*(q+1)/2 # number of triangular elements in the cross-covariance matrix 

# coords are observation coordinates for spLM()
coords <-cbind(runif(n,0,1),runif(n,0,1))
 

#Parameters for the bivariate spatial random effects
theta <-rep(3/0.5,q)
# Lower-triangular matrix for cross-covariance of Gaussian process
A <-matrix(0,q,q)
A[lower.tri(A,TRUE)] <-c(1,-1,0.25)
K <- A%*%t(A)

# dispersion matrix
Psi <-diag(0,q)

# calculate spatial covariance matrix
C <-mkSpCov(coords, K, Psi, theta, cov.model="exponential")

# Gaussian spatial process
w <-rmvn(1,rep(0,nrow(C)), C)
# w.1 and w.2 are every other element in w
w.1 <- w[seq(1,length(w),q)]
w.2 <- w[seq(2,length(w),q)]

# Covariate portion of the mean
x.1 <-cbind(1,rnorm(n))
x.2 <-cbind(1,rnorm(n))

# create a multivariate design matrix given q univariate design matrices
x <-mkMvX(list(x.1, x.2))

# parameters
B.1 <-c(1,-1)
B.2 <-c(-1,1)
B <-c(B.1, B.2)

# dispersion
Psi <-diag(c(0.1, 0.5))

# outcomes
y <-rnorm(n*q, x%*%B+w,diag(n)%x%Psi)

# outcomes based on every other element in y
y.1 <- y[seq(1,length(y),q)]
y.2 <- y[seq(2,length(y),q)]

# Call spMvLM
A.starting <-diag(1,q)[lower.tri(diag(1,q), TRUE)]

# number of MCMC iterations for spMvLM()
n.samples <- 1000

# tags with starting values for parameters for spMvLM()
starting <-list("phi"=rep(3/0.5,q), "A"=A.starting, "Psi"=rep(1,q))

# tags with Metropolis sampler variances for spMvLM()
tuning <-list("phi"=rep(1,q), "A"=rep(0.01,length(A.starting)), "Psi"=rep(0.01,q))

# tags with prior values
priors <-list("beta.Flat", "phi.Unif"=list(rep(3/0.75,q),rep(3/0.25,q)),
              "K.IW"=list(q+1,diag(0.1,q)), "Psi.ig"=list(c(2,2),c(0.1,0.1)))

# call spMvLM() function
m.1 <-spMvLM(list(y.1~x.1-1, y.2~x.2-1),coords=coords, starting=starting, tuning=tuning, 
             priors=priors,n.samples=n.samples, cov.model="exponential", n.report=100)