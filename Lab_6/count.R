#################################################################
# count data, over dispersion and zero inflation
# Lab 6, 13.10.14
# Quant III, Elad Zippory
#################################################################

rm(list=ls())
set.seed(123)
library(foreign)

# Overdispersion and zero inflation - POISSION and BINOMIAL
data <- read.dta("soccer_data.dta")
# Miguel, Edward, Sebastin M. Saiegh, and Shanker Satyanath. 
# "Civil war exposure and violence." Economics & Politics 23.1 (2011): 59-73.

## estimating poisson model
#---------------------------------------------------------------------
summary(poisson <- glm(yellow_card ~ civwar + age, 
                       family = "poisson", data = data))

## negative binomial
library(MASS)
summary(negbin <- glm.nb(yellow_card ~ civwar + age, data = data))

## estimating marginal effects of age using simulation, assuming civwar=0
#---------------------------------------------------------------------
par(mfrow=c(1,2))
## POISSON
betas <- mvrnorm(n=1000, mu=poisson$coefficients, 
                 Sigma=summary(poisson)$cov.unscaled)
age <- 18:40
marg <- matrix(NA, nrow=1000, ncol=length(age))

# this is a horrible double loop. try to do both with apply().
# i.e write a function of one iteration, with apply(), then another
# apply that calls that function.
for (i in 1:1000)
{
    for (a in 1:length(age))
    {
        marg[i,a] <- betas[i,3] * exp(c(1, 0, a) %*% betas[i,])
    }
}

marg <- apply(marg, 2, function(x)
  c(mean(x), quantile(x, c(.025, .975))))

plot(age, marg[1,], type="l", 
     xlab="Age", ylab="PR(Yellow Card)", ylim=c(0.02, 0.07))
lines(age, marg[2,], col="grey")
lines(age, marg[3,], col="grey")

#---------------------------------------------------------------------
## NEGATIVE BINOMIAL
betas <- mvrnorm(n=1000, mu=negbin$coefficients, 
                 Sigma=summary(negbin)$cov.unscaled)
age <- 18:40
marg <- matrix(NA, nrow=1000, ncol=length(age))
for (i in 1:1000)
{
    for (a in 1:length(age))
    {
        marg[i,a] <- betas[i,3] * exp(c(1, 0, a) %*% betas[i,])
    }
}

marg <- apply(marg, 2, function(x) c(mean(x), quantile(x, c(.025, .975))))

plot(age, marg[1,], type="l", 
     xlab="Age", ylab="", ylim=c(0.02, 0.07))
lines(age, marg[2,], col="grey")
lines(age, marg[3,], col="grey")
#---------------------------------------------------------------------
# Infalted Zeros?
par(mfrow=c(1,1))
hist(data$yellow_card)

# How would you test for it?
# given data and parameters, what is the probability that we observe zeros?

betas <- mvrnorm(n=1000, mu=poisson$coefficients, 
                 Sigma=summary(poisson)$cov.unscaled)

zeros=numeric(1000)
for (i in 1:1000)
{
    xb=cbind(1,data$civwar,data$age)%*%betas[i,]
    zeros[i]=sum(rpois(n = length(xb),lambda = exp(xb))==0)
}

plot(density(zeros),xlim = c(400,2000))
abline(v = sum(data$yellow_card==0))


betas <- mvrnorm(n=1000, mu=negbin$coefficients, 
                 Sigma=summary(negbin)$cov.unscaled)

zeros=numeric(1000)
for (i in 1:1000)
{
    xb=cbind(1,data$civwar,data$age)%*%betas[i,]
    zeros[i]=sum(rnegbin(n = length(xb),mu = exp(xb),theta = summary(negbin)$theta)==0)
}

plot(density(zeros),xlim = c(1000,2000))
abline(v = sum(data$yellow_card==0))
# so NB is plausible, but poor.

# Lab work: model over-dispersion and zero-inflation
