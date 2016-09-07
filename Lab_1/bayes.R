#################################################################
# Simulating Bayes Rule
# Lab 1, 8.9.14
# Quant III, Elad Zippory
#################################################################


# Aa motivation for simlations (and for those who do not buy the importance of 
# informative priors), let's say I don't know Bayes rule. Here is a classic example:
# A doctor tells a patient: I got bad news and good news. The bad news is that 
# according to a blood test you have cancer, with a true positive probability of .98.
# The good news is this cancer is very rare (1/30k)

# So, the probabilities are:
# P(D=1)=1/30k
# P(D=0)=1-1/30k
# P(T=1|D=1)=0.98
# P(T=1|D=0)=0.02

# WTS: (D=1|T=1)
# but we do not know\believe Bayes rule and want to do a simulation.

rm(list=ls())
set.seed(1212)

# size of the population
N=100000

# the two variables of interest
diagnosis=numeric(N)
sick=numeric(N)

# probability of cancer
pie_sick=1/30000
pie_test=0.98

# numer of iterations
ITER=1000
x=numeric(ITER)

for (i in 1:ITER)
{
  sick=rbinom(n = N,size = 1,pie_sick)
  
  diagnosis[which(sick==1)]=rbinom(n=length(which(sick==1)),size=1,prob=pie_test)
  diagnosis[which(sick==0)]=rbinom(n=length(which(sick==0)),size=1,prob=(1-pie_test))
  
  x[i]=sum(sick[diagnosis==1]==1)/sum(diagnosis==1)
  
}


mean(x) # compare with the analytical solution
sd(x)
plot(density(x))
hist(x,breaks = 100)

# weird right? what is the reason for this? 
# ANS: P(D=1) is very small, which creates sampling issues for such a small population.
# increase the population to get a proper distribution, but keep in mind that the mean is correct.


  