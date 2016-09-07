#################################################################
# Bayesian Model Averaging
# Lab 12, 24.11.14
# Quant III, Elad Zippory
#################################################################


# Bayesian Model Averaging
# let \gamma index models. then
# P(M_\gamma|X,y) is proportional to P(y|M_\gamma,X)P(M_\gamma)
# The number of models is the number of unique permutations of the number of vars.
# if k is the number of vars, 2^k is the number of models
# The posterior of a given parameter \theta is
# P(\theta|y,X)=\sum^{2^k}_\gamma P(\theta|M_\gamma,y,X)P(M_\gamma|X,y)

# why are we doing this?
# 1. dimension reduction
# 2. predictive modelling


rm(list=ls())

# install.packages("BMS")
library(BMS)


# Example 1: attitude data
data(attitude)
head(attitude)

?bms
fit <- bms(attitude,mprior="uniform")
coef(fit,std.coefs = T)
# do you see something odd?
# try to find an answer, becauase of the following probable scenario
# 1. Almighty Adviser told you to try Bayesian Model Averaging.
# 2. The best package you found is BMS
# 3. This does not make sense and you am all alone with the manual.

# let's inspect further
topmodels.bma(fit)[,1:3]
image(fit)
image(fit,yprop2pip = T)

beta.draws.bma(fit)[,1:3]
coef(fit,std.coefs = F)

# TRY: what betas would you get with some penalities?


# one simple way to play with priors per var?
fit2 <- bms(attitude,mprior="pip",mprior.size = c(.1,.5,.5,.5,.5,.5))

#let's compare
plotComp(fit,fit2)


# what happens when ncol(data)=25?

# solution: MCMC
# Example 2:
data(datafls)

fit <- bms(datafls,burn = 50000,iter=10000,g="BRIC",mprior = "uniform",nmodel=2000,mcmc="bd")
# see Corr PMP
fit <- bms(datafls,burn = 50000,iter=500000,g="BRIC",mprior = "uniform",nmodel=2000,mcmc="bd")


image(fit,yprop2pip = T)
plotConv(fit[1:20])


#############################
# BLA for GLM
#############################
# install.packages("BMA")
library(BMA)
data(attitude)
?bicreg
fit <- bicreg(x = attitude[,2:7],y = attitude[,1])


# Lab Work: now, let's try to do a logit
#----------------------------------------
# install.packages("car")
library(car)
data(Chile)
head(Chile)


