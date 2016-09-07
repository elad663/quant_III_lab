#################################################################
# Basic time-series. TS is nasty. this is the tip of the iceberg.
# Lab 8, 27.10.14
# Quant III, Elad Zippory (most of the code taken from Pablo 2013)
#################################################################

# install.packages(c("lmtest","urca","car"))

# no serial correlation and no lagged covariates)
#--------------------------------------------------

rm(list=ls())

set.seed(123)

# generating simulated data
t <- 200
x <- runif(t, -1, 1)
e <- rnorm(t, 0, 1)
beta <- 3

y <- beta * x + e

plot(ts(y))
abline(h = 0)


# OLS is unbiased and efficient
summary(ols <- lm(y ~ x))

plot(ts(ols$residuals))
abline(h = 0)

# testing for serial correlation
library(car)
durbinWatsonTest(ols, max.lag=1)
# note: H0 = no autocorrelation
# D-W Stats near 2 imply we do not reject the null

library(lmtest)
bgtest(ols)
dwtest(ols)

# Lab work:
#------------------------------------
# let's try to implement the BG test.
# Hints:
#   1) ols$resid
#   2) summary(ols)$r.squared
#   3) qchisq()



# no serial correlation and lagged covariates)
#--------------------------------------------------

rm(list=ls())
set.seed(777)

t <- 200
## now using 'ts' function (this will make computing lags easier)
x <- ts(runif(t, -1, 1))
l.x <- lag(x, -1)
e <- ts(rnorm(t, 0, 1))
beta <- 3
gamma <- 1
y <- beta * x + gamma * l.x + e

## putting it all together
d <- ts.union(y, x, l.x, e)
plot(d)

# everything still works...
summary(ols <- lm(y ~ x + l.x, data=d))
bgtest(ols)

summary(ols <- lm(y ~ x, data=d))
bgtest(ols)


# serial correlation and no lagged covariates)
#--------------------------------------------------

rm(list=ls())
set.seed(444)

t <- 200
x <- ts(runif(t, -1, 1))
rho <- 0.9
# generating AR1 errors
nu <- ts(rnorm(t))

beta <- 3

y <- beta * x + nu
for (i in 2:t){ y[i] <- rho*y[i-1] + y[i] }


d <- ts.union(y, x, nu)
plot(d) ## what plots show serial correlation?

summary(ols <- lm(y ~ x, data=d))

library(lmtest)
dwtest(ols)
bgtest(ols)

l.y <- lag(y, -1)
d <- ts.union(y, x, l.y, nu)
summary(ols_lagged <- lm(y ~ l.y + x, data=d))

bgtest(ols_lagged)


# spurious regression
#--------------------------------------------------

# two unrelated nonstationary series, if we use OLS we find a strong correlation

rm(list=ls())
set.seed(333)
t <- 500
x <- rnorm(t)
for (i in 2:t){ x[i] <- x[i-1] + rnorm(1) } ## x is I(1)
y <- rnorm(t)
for (i in 2:t){ y[i] <- y[i-1] + rnorm(1) } ## y is also I(1)

## both non-stationary
library(urca)
summary(ur.df(x, type="drift", lags=0))
summary(ur.df(x, type="none", lags=0))

par(mfrow=c(1,2))
plot(y, type="l")
plot(x, type="l")
par(mfrow=c(1,1))

## spurious regression
summary(lm(y ~ x))

set.seed(123)
TT=c(100,500,1000,10000,100000)
for (t in TT)
{
  x <- rnorm(t)
  for (i in 2:t){ x[i] <- x[i-1] + rnorm(1) }
  y <- rnorm(t)
  for (i in 2:t){ y[i] <- y[i-1] + rnorm(1) }
  
  # insane t-values
  print(summary(lm(y ~ x))$coefficients[2,3])
}

