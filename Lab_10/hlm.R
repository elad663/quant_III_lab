#################################################################
# hierarchical models in stan 
# Lab 10, 10.11.14
# Quant III, Elad Zippory
#################################################################

#-----------------------------------------------------------------------------
# recap:
# The intro showed you how to run a ols and poisson regression in rstan
# now, we are going to do:
# 1. simple partial pooling (no regressors), with informative priors
# 2. Random intercepts model
#-----------------------------------------------------------------------------

# 1. Predicting baseball batting averages via a hierarchical model
# (classic examples that shows Bayesian models outperforming MLE)
# See pages 310-316 Jackman

rm(list=ls())

library(pscl)
library(foreign)
library(rstan);rstan_options(auto_write = TRUE);options(mc.cores = 3)


# Function to convert stan fit objects to coda objects
stan_to_coda <- function(fit){
  t <- extract(fit, permuted=FALSE, inc_warmup=FALSE)
  mcmc <- mcmc.list(lapply(1:ncol(t), function(x) mcmc(t[,x,])))
  return(mcmc)
}

data(EfronMorris)

d <- EfronMorris

# codebook:
?EfronMorris

# MLE estimate is just batting average
MLE_rmse <- sqrt(mean((d$y-d$p)^2))
MLE_rmse

# Hierarchical model ('partial pooling')

# each observed batting average y_i is drawn from a normal density with
# unknown mean theta_i; and fixed variance sigma:
sigma <- sqrt(mean(d$y) * (1-(mean(d$y)))/45)

# Each theta_i (the true batting average of each player; which we 
# would observe  if they kept playing forever) is also drawn 
# from a normal distribution with unknown mean mu and variance tau.

# Then we give priors to the hyperparameters mu and tau.
# for mu, we know it must be somewhere between 0.15 and .30; so we 
# choose values that give a 95% credible interval: normal(0.225, 0.0375)
# see Jackson p.311 for Baseball domain knowledge...

x <- seq(0, 0.5, 0.001)
plot(x, dnorm(x, mean=0.225, sd=0.0375), type="l")

# for tau, we choose a gamma distribution (why? it's always positive
# and it's a conjugate prior for a normal distribution)

# what prior belief do we have about tau, 
# the standard deviation of the thetas?
# the sd of the observed y is:
sd(d$y)

# it must be somewhere around 0.07; probably higher; let's say 0.10
# so MEAN of values we draw from gamma distribution must be around 0.10
# gamma distribution has mean shape/rate [OR shape*scale]

# so let's follow Jackman and choose shape=7; then rate=7/0.10 ~= 70
x <- seq(0.001, 1, 0.001)
plot(x, dgamma(x, shape=7, rate=70), type="l")

hierarchical_code <- '
data {
  int<lower=0> N; # observations
  real y[N]; # outcome variable
  real<lower=0> sigma; # fixed variance of observed batting averages
}
parameters {
  real theta[N]; ## unobserved true batting averages
  real mu; ## hyperparameter: mean of batting averages
  real<lower=0> tau; ## hyperparameter: variance of batting averages
}
model {
  mu ~ normal(0.225, 0.0375); ## prior about hyperparameter (overall batting average)
  tau ~ gamma(7, 70); ## priors about variance of batting averages
  for (n in 1:N){;
  y[n] ~ normal(theta[n], sigma);
  theta[n] ~ normal(mu, tau);
}
}
'

data <- list(N=length(d$y), y=d$y, sigma=sigma)
fit <- stan(model_code=hierarchical_code, data=data, iter=10000, chains=4)

# summary results
monitor(fit, digits_summary=3)

# Assessing convergence
# 1) Looking at traceplots
traceplot(fit, pars='theta', inc_warmup=FALSE, ask=TRUE)

library(coda)
# 4) Geweke tests
mcmc <- stan_to_coda(fit)
geweke.diag(mcmc)
geweke.plot(mcmc, ask=TRUE)

# 5) Heidelberger-Welch test of non-stationarity
heidel.diag(mcmc)

# 6) looking at serial correlation in chains
autocorr(mcmc)

autocorr.plot(mcmc, ask=TRUE)


# computing rmse
estimates <- summary(fit)
predicted <- estimates$summary[1:18, 'mean']
rmse_MCMC <- sqrt(mean((predicted-d$p)^2))
rmse_MCMC

# visualizing results (replication of Figure 7.4 in page 316 Jackman)
actual <- data.frame(batter = d$name, average=d$p, estimate="Actual")
bayes <- data.frame(batter = d$name, average=predicted, estimate="Bayes")
mle <- data.frame(batter = d$name, average=d$y, estimate="MLE")

df <- rbind(actual, bayes, mle)

library(ggplot2)
p <- ggplot(df, aes(x=estimate, y=average, group=batter))
p + geom_point() + geom_line() + theme_bw() + coord_flip() +
    scale_x_discrete(expand=c(0.05,0.05)) +
    scale_y_continuous("Batting average") + 
    theme(axis.line.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_blank(),
        axis.line = element_line(size = 0.50)) +
    geom_vline(xintercept=1, alpha=1/3, size=0.2) +
    geom_vline(xintercept=2, alpha=1/3, size=0.2) +
    geom_vline(xintercept=3, alpha=1/3, size=0.2)


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

#2. Random Intercepts
# Inspired by: http://www.princeton.edu/~jkastell/mrp_primer.html

rm(list=ls())

library(pscl)
library(foreign)
library(rstan);rstan_options(auto_write = TRUE);options(mc.cores = 3)


# Function to convert stan fit objects to coda objects
stan_to_coda <- function(fit){
  t <- extract(fit, permuted=FALSE, inc_warmup=FALSE)
  mcmc <- mcmc.list(lapply(1:ncol(t), function(x) mcmc(t[,x,])))
  return(mcmc)
}


## polls about gay rights ("ground truth"), from Lax and Phillips (2009)
d <- read.dta("lax_phillips_gay_rights.dta", convert.underscore = TRUE) 

# delete missing data in variables of interest
d <- d[!is.na(d$yes.of.all) & !is.na(d$age) & (d$state!="") & !is.na(d$region),]

# computing means by state
actual.means <- aggregate(d$yes.of.all, by=list(d$state), mean)$x

# computing average age by state
age_means <- aggregate(d$age, by=list(d$state), mean)$x

# take random sample of N=1000 (for computation purposes)
# set.seed(1234)
# d <- d[sample(1:length(d$state), 1000),]

# and let's see how well we can replicate "true" support for gay marriage
# in each state using a random sample

# Variables at the individual level
y <- d$yes.of.all 
age <- d$age
state <- d$state
states <- sort(unique(state))
ss <- match(state, states) ## number of state to which each obs. belongs

################################################################
## RANDOM INTERCEPTS MODEL
################################################################

multilevel_code <- '
data {
  int<lower=0> N; # number of respondents
  int<lower=0> S; # number of states
  int<lower=0, upper=1> y[N]; # outcome variable
  int<lower=1,upper=S> ss[N]; # state for each observation
  real age[N];
  real age_means[S];
}
parameters {
  real alpha[S]; # state-specific intercept
  real beta; # effect of age
  real mu; # mean of intercepts
  real<lower=0> tau; # sd of intercepts
}
model {
  beta ~ normal(0, 100);
  mu ~ normal(0, 100);
  tau ~ gamma(1, 2);
  for (s in 1:S)
  alpha[s] ~ normal(mu, tau);
  for (n in 1:N)
  y[n] ~ bernoulli(inv_logit(alpha[ss[n]] + beta * age[n]));
}
generated quantities {
  real predictions[S];
  for (s in 1:S)
  predictions[s] <- inv_logit(alpha[s] + beta * age_means[s]);
}
'
data <- list(N=length(y), S=length(states), y=y,
             ss=ss, age=age, age_means=age_means)

fit <- stan(model_code=multilevel_code, data=data, iter=1000, chains=3)

# results and convergence statistics
monitor(fit, digits_summary=2, warmup=100)

traceplot(fit, ask=TRUE)

# compute observed means
observed.means <- aggregate(y, by=list(state), mean)$x

# compute predicted means by Bayesian model
estimates <- summary(fit)
varnames <- paste0('predictions[', 1:49, ']')
predicted.means <- estimates$summary[varnames, 'mean']


################################################################
## MODELING RANDOM INTERCEPTS
################################################################

# Variable at the state level: vote for Kerry in 2004 in each state
kerry <- c(36.7999992370605, 44.5999984741211, 44.4000015258789, 54.2999992370605, 
           47, 54.2999992370605, 89.1999969482422, 53.4000015258789, 47.0999984741211, 
           41.4000015258789, 49.2000007629395, 30.2999992370605, 54.7999992370605, 
           39.2999992370605, 36.5999984741211, 39.7000007629395, 42.2000007629395, 
           61.9000015258789, 55.9000015258789, 53.5999984741211, 51.2000007629395, 
           51.0999984741211, 46.0999984741211, 39.7999992370605, 38.5999984741211, 
           43.5999984741211, 35.5, 32.7000007629395, 50.2000007629395, 52.9000015258789, 
           49.0999984741211, 47.9000015258789, 58.4000015258789, 48.7000007629395, 
           34.4000015258789, 51.4000015258789, 50.9000015258789, 59.4000015258789, 
           40.9000015258789, 38.4000015258789, 42.5, 38.2000007629395, 26, 
           45.5, 58.9000015258789, 52.7999992370605, 49.7000007629395, 43.2000007629395, 
           29.1000003814697)

multilevel_code <- '
data {
  int<lower=0> N; # number of respondents
  int<lower=0> S; # number of states
  int<lower=0, upper=1> y[N]; # outcome variable
  int<lower=1,upper=S> ss[N]; # state for each observation
  real age[N];
  real kerry[S];
  real age_means[S];
}
parameters {
  real alpha[S]; # state-specific intercept
  real beta; # effect of age
  real gamma; # effect of kerry
  real mu; # mean of intercepts
  real<lower=0> tau; # sd of intercepts
}
model {
  beta ~ normal(0, 100);
  gamma ~ normal(0, 100);
  mu ~ normal(0, 100);
  tau ~ gamma(1, 2);
  for (s in 1:S)
  alpha[s] ~ normal(mu + gamma * kerry[s], tau);
  for (n in 1:N)
  y[n] ~ bernoulli(inv_logit(alpha[ss[n]] + beta * age[n]));
}
generated quantities {
  real predictions[S];
  for (s in 1:S)
  predictions[s] <- inv_logit(alpha[s] + beta * age_means[s]);
}
'


data <- list(N=length(y), S=length(states), y=y,
    ss=ss, age=age, age_means=age_means, kerry=kerry)

fit <- stan(model_code=multilevel_code, data=data, iter=1000, chains=3)

# results and convergence statistics
monitor(fit, digits_summary=2, warmup=100)
#traceplot(fit, ask=TRUE)


