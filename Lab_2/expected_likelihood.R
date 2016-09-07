#################################################################
# Expected likelihood
# Lab 2, 15.9.14
# Quant III, Elad Zippory
#################################################################

rm(list=ls())
set.seed(123)

# Let's define a function that is the log likelihood for the Poisson
#y=Y;lambda=1
log.likelihood.poisson <- function(y,lambda){
  n=length(y)
  log_lk=log(lambda)*sum(y)-n*lambda-sum(log(factorial(y)))
  return(-log_lk)
} 

# Let's generate some data...
N=1000;true_lambda=2
Y=rpois(n = N,lambda = true_lambda)

results=optim(fn = log.likelihood.poisson,par = runif(n = 1,min = 0,max = 20),y=Y,
      method = "BFGS",hessian = T,control = list(trace=1))

str(results)
# so what is the standard error?
# recall E[H(\theta)]^-1=var[\theta]
solve(results$hessian)


# Now, let's see what is the expected likelihood
# we are going to resample K times, and get MLE for each time.
K=10000;N=1000;lambda_hat=numeric(K)
for (i in 1:K)
{
  Y=rpois(n = N,lambda = true_lambda)  
  lambda_hat[i]=optim(fn = log.likelihood.poisson,
                      par = 1,y=Y,
                      method = "BFGS",hessian = T,control = list(trace=0))$par
}

plot(density(lambda_hat))
abline(v =true_lambda)

mean(lambda_hat)
var(lambda_hat)

