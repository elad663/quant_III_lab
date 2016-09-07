#################################################################
# data manipulation with dplyr and data.table
# Lab 11, 17.11.14
# Quant III, Elad Zippory
#################################################################


rm(list=ls())

library(pscl)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = 3)

# loading roll call data
# Source: http://jackman.stanford.edu/blog/
load("senate_rc.rda")
rc <- dropRollCall(rc, dropList=list(codes = "notInLegis", lop = 0))


stan.code <- '
data {
  int<lower=1> J; // number of legislators
  int<lower=1> K; // number of bills
  int<lower=1> N; // number of observations
  int<lower=1,upper=J> j[N]; // legislator for observation n
  int<lower=1,upper=K> k[N]; // bill for observation n
  int<lower=0,upper=1> y[N]; // vote of observation n
}
parameters {
  real alpha[K];             
  real beta[K];   
  real theta[J];
}
model {
  alpha ~ normal(0, 25);
  beta ~ normal(0, 25);
  theta ~ normal(0, 1);
  for (n in 1:N)
    y[n] ~  bernoulli_logit( theta[j[n]] * beta[k[n]] - alpha[k[n]] );
}
'

J <- dim(rc$votes)[1]
K <- dim(rc$votes)[2]
N <- length(rc$votes)
j <- rep(1:J, times=K)
k <- rep(1:K, each=J) 
y <- c(rc$votes)

# deleting missing values
miss <- which(is.na(y))
N <- N - length(miss)
j <- j[-miss]
k <- k[-miss]
y <- y[-miss]

## data and initial values
stan.data <- list(J=J, K=K, N=N, j=j, k=k, y=y)

# obviously with higher iterations and chains
stan.fit <- stan(model_code=stan.code, data=stan.data, iter=500, warmup=200,
                 chains=1, thin=2)

load("stan_irt.Rdata")


# basic convergence; do the rest of the tests
traceplot(stan.fit, pars='theta', ask=TRUE)

################################################################
## IRT WITH COVARIATES
################################################################

# Different ways of doing this...
# With Stan, it would be something like this:


stan.code <- '
data {
  int<lower=1> J; // number of legislators
  int<lower=1> K; // number of bills
  int<lower=1> N; // number of observations
  int<lower=1,upper=J> j[N]; // legislator for observation n
  int<lower=1,upper=K> k[N]; // bill for observation n
  int<lower=0,upper=1> y[N]; // vote of observation n
  real party[J]; // party of legislator j (0 for D/I, 1 for R)
}
parameters {
  real alpha[K];             
  real beta[K];   
  real theta[J]; # realized ideology
  real gamma[J]; # unobserved, true ideology
  real beta_party; # effect of party ID
}
model {
  alpha ~ normal(0, 5); 
  beta ~ normal(0, 5);
  beta_party ~ normal(0, 2);
  for (i in 1:J){
    theta[i] ~ normal(gamma[i], 1); # hierarchical structure for obs. ideol.
    gamma[i] ~ normal(beta_party * party[i], 1); # effect of party ID
  };
  for (n in 1:N)
    y[n] ~  bernoulli_logit( theta[j[n]] * beta[k[n]] - alpha[k[n]] );
}
'

repub <- ifelse(rc$legis.data$party=="R", 0, 1)
stan.data <- list(J=J, K=K, N=N, j=j, k=k, y=y, party=repub)

stan.fit <- stan(model_code=stan.code, data=stan.data, iter=500, warmup=200,
                 chains=1, thin=2)

monitor(stan.fit)
