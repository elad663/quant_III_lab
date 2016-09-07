#################################################################
# intorduction to stan
# Lab 10, 10.11.14
# Quant III, Elad Zippory
#################################################################


# install.packages(c("pscl","rstan","mvtnorm","coda"))

rm(list=ls())

library(rstan)
library(mvtnorm)
library(MASS)


# a toy example
#-----------------------------
y <- c(0, 0)
rho <- 0.9
Sigma <- matrix(c(1, rho, rho, 1), nrow=2)

hmc_code <- '
    data {
        vector[2] y; // data (means)
        matrix[2,2] Sigma; // covariance matrix (known)
    }
    parameters {
        vector[2] theta; // parameters of bivariate normal
    }
    model {
        y ~ multi_normal(theta, Sigma);
    }
'

data <- list(y=y, Sigma=Sigma)
fit <- stan(model_code=hmc_code, data=data, iter=500, chains=4)

# summary results
monitor(fit)

# The last line of this output, lp__, is the logarithm of the (unnormalized)
# posterior density as calculated by Stan while performing the Hamiltonian Monte
# Carlo algorithm. This log density can be used in various ways for model evaluation
# and comparison.

#------------------------------------------------------------------

# Poisson
#-----------------------------
rm(list=ls())

x <- mvrnorm(n=50, mu=c(-0.5,0,0.5), Sigma=diag(3))
true.beta <- c(-1, 0, 1)
y <- rpois(n=50, lambda=exp(x%*%true.beta))

hmc_code <- '
    data {
        int<lower=0> N; # observations
        int<lower=0> K; # variables
        int y[N];       # data (integers)
        row_vector[K] x[N];  # covariates
    }
    parameters {
        vector[K] beta;  ## coefficients to be estimated
    }
    model {
      for (k in 1:K)
        beta[k] ~ cauchy(0, 2.5); ## priors on betas
      for (n in 1:N)
        y[n] ~ poisson(exp(x[n] * beta)); ## model for y
    }
'

data <- list(N=length(y), K=dim(x)[2], y=y, x=x)
fit <- stan(model_code=hmc_code, data=data, iter=500, chains=4)

# summary results
monitor(fit)

# convergence -- more on this soon.
traceplot(fit, pars='beta')

#------------------------------------------------------------------

# Regression
#-----------------------------
rm(list=ls())


x <- runif(100)
beta <- 3
alpha <- 1
y <- alpha + beta * x + rnorm(100)

hmc_code <- '
    data {
        int<lower=0> N; # observations
        vector[N] x; # covariate
        vector[N] y; # outcome variable
    }
    parameters {
        real alpha;  # intercept
        real beta;   # slope
        real<lower=0> sigma; # error
    }
    model {
        alpha ~ normal(0, 100);
        beta ~ normal(0, 100);
        for (n in 1:N)
          y[n] ~ normal(alpha + beta * x[n], sigma);
    }
'

data <- list(N=length(y), x=x, y=y)
fit <- stan(model_code=hmc_code, data=data, iter=50000, chains=4)

summary(lm(y~x))

monitor(fit,digits_summary = 3)

traceplot(fit)


stan_to_coda <- function(fit){
  # Taken From Pablo Barbera
  t <- extract(fit, permuted=FALSE, inc_warmup=FALSE)
  mcmc <- mcmc.list(lapply(1:ncol(t), function(x) mcmc(t[,x,])))
  return(mcmc)
}

library(coda)

fit_mcmc <- stan_to_coda(fit)

# Z scores of converges.
geweke.diag(fit_mcmc) 
geweke.plot(fit_mcmc, ask=TRUE)

heidel.diag(fit_mcmc)

autocorr(fit_mcmc)
autocorr.plot(fit_mcmc, ask=TRUE)




