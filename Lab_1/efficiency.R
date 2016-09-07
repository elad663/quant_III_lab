#################################################################
# Re-introduction to R; pitfalls and efficiency
# Lab 1, 8.9.14
# Quant III, Elad Zippory
#################################################################

rm(list=ls())

# Floating Point
(.1 == .3 / 3)
# this will ruin your conditions. use round().
#--------------------------------------

# memory movements
vec <- numeric(0)
n=10000
system.time({for(i in 1:n) vec <- c(vec, i)})

vec <- numeric(n)
system.time({vec <- 1:n}) 

# The larger your object in memory, the more time the OS will need to move it.
# So, always pre-specify the size of your objects. See R inferno for what
# to do when the final size is unknown.
# And, always look for a base function.
# rbind and cbind are evil. Plan your objects. Construct them. Fill them.
#--------------------------------------


# vectorize your code and use base functions. 
# it will give you the real performance R can achieve
z=rnorm(n = 100000,mean = 2)
y=rnorm(n = 100000,mean = 4)
w=abs(z)

# bad:
lsum <- 0
for(i in 1:length(w)) 
{
  lsum <- lsum + log(w[i])  
}

# good:
lsum <- sum(log(w))
#--------------------------------------

# but let's say we don't have a base function for what we want.
# e.g. we want that all the positive numbers should be NA

# bad:
x=z
for (i in 1:length(x))
{
  if (x[i]>0) x[i]=NA
}

# or
x=z
x[x>0]=NA
which(x>0)
#--------------------------------------

# now, let's do it for a matrix...

# this can get interesting quickly.
mat=cbind(z,y)
mat_v=mat
mat_l=mat

head(mat)
str(mat) # str will help you understand what you are looking at.



# bad:
for (j in 1:dim(mat_l)[2])
{
  for (i in 1:dim(mat_l)[1])
  {
    if (mat_l[i,j]>0) mat_l[i,j]=NA
  }
}

# good:
str(which(mat_v>0,arr.ind = T))
# "which(mat_v>0,arr.ind = T)" is a matrix of indices.
mat_v[which(mat_v>0,arr.ind = T)]=NA
#--------------------------------------



# R functions are like math functions 
# (this snippet was stolen from Pablo Barbera)
#---------------------------------------------------------------

## a trivial example
square_prod <- function(x,y){
  return((x * y)^2)
}

square_prod(2, 1)
square_prod(5, 10)

## computing OLS estimates with matrix algebra
ols <- function(y, X){
  if (all(X[1,]!=1)){ X <- cbind(1, X) }
  beta <- solve(t(X)%*%X)%*%t(X)%*%y
  rownames(beta)[1] <- "(Intercept)"
  return(round(t(beta),3))
}

## simulating data
x1 <- rnorm(100, 0, 1)
x2 <- rnorm(100, 0, 1)
y <- 5 + 3 * x1 + -1 * x2 + rnorm(100, 0, 1)
X <- cbind(x1, x2)

## running function and checking with R built-in estimator
ols(y, X)
lm(y ~ x1 + x2)
#---------------------------------------------------------------


