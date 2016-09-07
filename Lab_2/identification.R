#################################################################
# Identification Issues in ML
# Lab 2, 15.9.14
# Quant III, Elad Zippory
#################################################################

rm(list=ls())
set.seed(123)

y <- rnorm(100)
# let's define the log likelihood for a normal.
# here we are using a ready made function - dnorm()
llik <- function(theta) -sum(dnorm(y, theta[1]+theta[2], 1, log=TRUE))
# we are going to avoid the loop from before, so we throw optim into
# another function. This is a not just matter of style:
# loops are readable to a point (ureadable code = bugs)
opt  <- function(start.value) optim(start.value, llik)$par
# replicate() is a member of lapply,sapply etc.
mle  <- replicate(100, opt(rnorm(2)))
# investigate this new creature by yourself with str()

# MLE values starting at different places
plot(t(mle))
# ... a perfect straight line.

# Let's look at the hessian... what do we expect to see?
optim(rnorm(2), llik, hessian=TRUE)


# Let's look at the log-likelihood surface:
# for this we need to do a nice grid mapping of the likelihood
# so we are going to evaluate the function many times...
# (if you do not understand the various commands, 
# explore them with tab or the help page)
# it is not your Q1 R code, make sure to get it.
L <- 100
theta  <- seq(-1/2, 1/2, length=L)
z      <- -apply(expand.grid(theta, theta), 1, llik)
dat   <- data.frame(z=z, theta=theta)

library(lattice)
wireframe(z ~ theta*theta, drape=TRUE, data=dat, xlab="theta1", 
          ylab="theta2", scale=list(arrows=FALSE))
filled.contour(theta, theta, matrix(z, ncol=L, nrow=L))
# so the maxima is actually a ridge. very sad.

# We would like to investigate the Hessian, but it is singular. 
# so we are going to use the simulated standard error.

set.seed(123)
iter=c(100,1000,10000,100000)

#runif(n = 2,min = -Inf,max = Inf)
for (i in iter)
{
  mle  <- replicate(i, opt(rcauchy(n = 2,scale = 10,location = 0)))  
  print(c(sd(mle[1,]),sd(mle[2,])))
}

# boom. we don't have anything when the parameters are not identified.
# not even standrad errors.



