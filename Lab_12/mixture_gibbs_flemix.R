#################################################################
# mixture models, manual gibbs sampler and flemix
# Lab 12, 24.11.14
# Quant III, Elad Zippory
#################################################################



rm(list=ls())

library(MCMCpack) # to sample from Dirichlet
library(mnormt) # to sample from multivariate normal

set.seed(314)
n <- 1000
K <- 2
x <- rnorm(n)
q <- replicate(n, sample(1:K, 1, prob = (1:K)/K))

# Intercept only (density estimation)
#y <- rnorm(n, -2*(q==1) + 2*(q==2), 1*(q==1) + 0.5*(q==2))

# Intercept and slope (GMM regression)
y <- rnorm(n, (-2 + x)*(q==1) + (2 - x)*(q==2), 1*(q==1) + 0.5*(q==2))

# Null model
#y <- rnorm(n, 0, 1)

hist(y, breaks=200, freq=FALSE)
lines(density(y, adjust=0.5), col="red", lwd=3)
plot(x = x,y=y)
X <- matrix(cbind(1, x), ncol=2, nrow=n)


#Assign units to clusters
#-------------------------------------------------------------------------------
update.z <- function(){
  
#   1. sample z
#   1.1 PR(z_i=k|y_i)= f(y_i|theta_k)*pie_k / sum_k{f(y_i|theta_k)pie_k}
#   1.2 z_i ~ multinomial({pie_1,...,pie_k},1)
  
  p <- function(w) dnorm(y, X%*%c(theta$b[[w]]), sqrt(theta$sigma2[[w]]))*pi[w]
  a <- sapply(1:K, p)
  a <- a/apply(a,1,sum)
  
#    rowSums(a)==apply(a,1,sum)
  
  sapply(1:n, function(x) sample(1:K, 1, a[x,], replace=FALSE))
}
#-------------------------------------------------------------------------------


#Sample parameters
#-------------------------------------------------------------------------------
update.theta <- function(){
  
  for (w in 1:K){
    a <- which(z == w)
    if (K == 1) a = 1:length(z)
    if (length(a) > 2){
      
      fit <- lm(y[a] ~ -1 + X[a, ])
      
      theta$b[[w]] <- c(
        rmnorm(1, fit$coef, solve(crossprod(X[a,]))*theta$sigma2[[w]]))
      
      e <- y[a] - X[a, ]%*%as.matrix(theta$b[[w]])
      theta$sigma2[[w]] <- 1/rgamma(1, shape = length(e)/2, rate = t(e)%*%e/2)
      
      # plot(density(rgamma(100000, shape = length(e)/2, rate = t(e)%*%e/2)))
      
    }
    
  }
  
  theta
}
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
update.pi <- function(){
  c(rdirichlet(1, c(table(factor(z))+ 1)))
}
#-------------------------------------------------------------------------------

#starting values:
pi <- rep(1/K, K)
theta <- list(b = rep(list(rep(0, ncol(X))), K), sigma2=rep(list(1), K))

M <- 1000


#preparing data structure for output
#-------------------------------------------------------------------------------
out <- list(
  theta = matrix(NA, M, length(unlist(theta))),
  z = matrix(NA, M, n),
  pi = matrix(NA, M, K)
)
colnames(out$theta) <- c(
  paste(rep(paste("b", 0:(ncol(X)-1), sep=""), K), rep(1:K, each=ncol(X)), sep="-"),
  paste("sigma2", 1:K, sep="-"))
#-------------------------------------------------------------------------------


for (i in 1:M){
  z     <- update.z()
  theta <- update.theta()
  pi    <- update.pi()
  K     <- sum(pi > 0.2)
  # if we include this line, what's the bug in the following lines?
  out$theta[i, ] <- unlist(theta)
  out$z[i, ] <- z
  out$pi[i, ] <- pi
  if (i%%100==0){
    matplot(out$theta, type="l", lwd=2, lty=1)
    Sys.sleep(0)
  }
}


# plot(apply(out$z, 2, mean), q)
plot.ts(out$theta)
plot.ts(out$pi)

# Predicted distribution for x = -1 and x = 1
predict_mixture <- function(out, x){

  temp <- function(t, x){
  yhat <- seq(min(y), max(y), length=100) #set of values on which to estimate density
  b <- out$theta[t,]
  # separate into component-wise estimates
  m <- unlist(lapply(strsplit(colnames(out$theta), "-"), function(x) x[2]))
  w <- grepl("sigma2", colnames(out$theta))
  
  apply(sapply(1:K, function(z) {
    out$pi[t,z]*dnorm(yhat, c(1, x)%*%b[which(m[!w]==z)], sqrt(b[which(w)[z]]))})
  ,1,sum)
}
 sapply((M/2):M, temp, x=x)
}


p1 <- predict_mixture(out, x=-1)
p2 <- predict_mixture(out, x=1)

yhat <- seq(min(y), max(y), length=100) #set of values on which to estimate density
plot(yhat, apply(p1, 1, mean), type="l", col="blue", lwd=2)
lines(yhat, apply(p2, 1, mean), type="l", col="red", lwd=2)

ci <- function(x) quantile(x, c(0.025, 0.5, 0.975))
R <- cbind(t(apply(p1, 1, ci)), t(apply(p2, 1, ci)))
matplot(yhat, R, type="l", col=c(rep("red", 3), rep("blue", 3)), lty=c(3,1,3,3,1,3), lwd=2)


#-------------------------------------------------------------------------------
#-------------------------          FLEXMIX         ----------------------------
#-------------------------------------------------------------------------------
library(flexmix)


m1=flexmix(y~1+X[,2],k=2)
m1

# let's asses the parameters
parameters(m1)
summary(refit(m1))

# let's compare this with Gibbs:
# let's recall the out object
colnames(out$theta) 
# the values are almost identical, but what's the difference in interpertation?
plot(density(out$theta[,c(2,4)])) 

summary(m1)
# remember:
table(q,clusters(m1))
# so not too bad!

# now, let's asses the probabilities
plot(m1)


# Lab Work, (if time permitting)
#-----------------------
# but, ALAS! in the gibbs sampler we did not retreive the PR(z_i=k|y_i)
# which is what flexmix shows us. but we did calculate it... 
# how would we retreive it?



