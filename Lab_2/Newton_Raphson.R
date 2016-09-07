#################################################################
# Newton Raphson optimization algorithm
# Lab 2, 15.9.14
# Quant III, Elad Zippory
#################################################################


# Newton Raphson for Poisson
rm(list=ls())
set.seed(123)
n <- 1000

y <- rpois(n = n,lambda =  2)

lambda <- seq(0.1, 5, length=1000)
plot(lambda, sapply(lambda, function(lambda) sum(y)/lambda-n),type = "l")
abline(h=0)

step <- function(lambda) lambda + (sum(y)/lambda-n)/(sum(y)/lambda^2)

s    <- .5
s    <- step(s)
s

# now in a while loop:
thrs <- .00000001
while (abs(step(s)-s)>thrs) 
{
  print(s)
  s <- step(s)
}
