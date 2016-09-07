#################################################################
# Predicted Probabilites and Marginal Effects for Logit
# Lab 3, 22.9.14
# Quant III, Elad Zippory
#################################################################


# Predicted Probabilites
#################################################################
rm(list=ls())
set.seed(123)

# version 1
probit.loglk_manual <- function(beta, X, y){
  loglk <- sum(y * pnorm(X%*%beta, log.p=TRUE) + 
                 (1-y) * pnorm(-(X%*%beta), log.p=TRUE))
  return(-loglk)
}

# version 2
probit.loglk_base <- function(beta, X, y) -sum(dbinom(y, 1, pnorm(X%*%beta),log=TRUE))

# generate data
n <- 1000
x <- rnorm(n)
z <- rnorm(n)
B=c(-1,2,-.5)
X=cbind(1,x,z)
y <- rbinom(n, 1, pnorm(X%*%B))

init=rnorm(n = ncol(X))
fit=optim(par = init,fn = probit.loglk_manual,method = "BFGS",X=X,y=y,hessian = T)
fit2=optim(par = init,fn = probit.loglk_base,method = "BFGS",X=X,y=y,hessian = T)

# Predicted Probabilities
predicted_probabilites=pnorm(X%*%fit$par)
plot(y=predicted_probabilites[order(x)],x=x[order(x)],type = "l")
# looks weird right?

# think of partial derivative - only one variable is moving.
XX=cbind(1,x,mean(z))
predicted_probabilites=pnorm(XX%*%fit$par)
plot(y=predicted_probabilites[order(x)],x=x[order(x)],type = "l")


# A more interesting data generating process
#----------------------------------------------------------------------
x=sign(x)*x^2
y <- rbinom(n, 1, pnorm(cbind(1,x,z)%*%B))
fit <- glm(y ~ x+z, family=binomial(link=probit))
predicted_probabilites=pnorm(cbind(1,x,mean(z))%*%fit$coefficients)
plot(y=predicted_probabilites[order(x)],x=x[order(x)],type = "l")
# what happended here?
# shall we ignore the "warning" that
# glm.fit: fitted probabilities numerically 0 or 1 occurred 
#----------------------------------------------------------------------

# back to the first case:
#----------------------------------------------------------------------
n <- 200
x <- rnorm(n)
z <- rnorm(n)
B=c(-1,2,-.5)
X=cbind(1,x,z)
y <- rbinom(n, 1, pnorm(X%*%B))

init=rnorm(n = ncol(X))
fit=optim(par = init,fn = probit.loglk_manual,method = "BFGS",X=X,y=y,hessian = T)

# Predicted Probabilities
XX=cbind(1,x,mean(z))
predicted_probabilites=pnorm(XX%*%fit$par)
plot(y=predicted_probabilites[order(x)],x=x[order(x)],type = "p")

# Remeber the ungly integral: Phi(x'b)=\integral Phi(xb_hat)f(b)db?

library(MASS) # for mvrnorm() [MASS is an important library]
# 1) Phi(1/k * sum(X'b)) --k--> Phi(E(X'b))     k: number of iterations
#----------------------------------------------------------------------
beta_hat <- mvrnorm(n = 100,mu = fit$par,Sigma=solve(fit$hessian))
p_hat <- beta_hat%*%t(XX) # this is not the probability yet
p_bar_1 <- pnorm(apply(p_hat,2,mean))

plot(y=p_bar_1[order(x)],x=x[order(x)],type = "p")
points(x, predicted_probabilites, col="red", cex=1)

# 2) 1/k sum[Phi(X'b)] --> E[Phi(X'b)] k: number of iterations
#----------------------------------------------------------------------
beta_hat <- mvrnorm(n = 100,mu = fit$par,Sigma=solve(fit$hessian))
p_hat <- pnorm(beta_hat%*%t(XX))
p_bar_2 <- apply(p_hat,2,mean)
points(y=p_bar_2[order(x)],x=x[order(x)], col="blue", cex=1)

# Lab Work I:
#----------------------------------------------------------------------
# 3) How about bootstrapping? resample X, not B. Let's do it together now.


# Actual Data - from Pablo Barbera
#----------------------------------------------------------------------
rm(list=ls())
set.seed(123)
# Pablo's toy Twitter data
db <- read.csv("lab3_data.csv", stringsAsFactors=F)

str(db)
summary(db)
head(db)

y=db[,1]
X=as.matrix(cbind(1,db[,2:6]))

# we can use optim...
probit.loglk_base <- function(beta, X, y) -sum(dbinom(y, 1, pnorm(X%*%beta),log=TRUE))
fit=optim(par = rnorm(n = ncol(X)),fn = probit.loglk_base,method = "BFGS",X=X,y=y,hessian = T)

# or glm
probit <- glm(twitter ~ age + female + high.school + college + nytimes,
              control=list(maxit = 250),
              data=db, family=binomial(link="probit"))


# predicted probabilities for the $average$ individual
# be sure to use mean/median/mode as necessary
newdata <- data.frame(age = mean(db$age), female=median(db$female),
                      high.school=median(db$high.school), 
                      college=median(db$college),
                      nytimes = median(db$nytimes))

pred <- predict(probit, newdata, type="response", se.fit=TRUE)
pred$fit

# confidence interval
c(pred$fit - (pred$se.fit * 1.96),pred$fit + (pred$se.fit * 1.96))

# predicted probabilities at different values of the age variable
newdata <- data.frame(age = 18:80, female=median(db$female),
                      high.school=median(db$high.school), 
                      college=median(db$college),
                      nytimes = median(db$nytimes))

pred <- predict(probit, newdata, type="response", se.fit=TRUE)

plot(18:80, pred$fit, type="l", xlab="age", ylab="Pr(Twitter=1)", ylim=c(0, .40))
lines(18:80, pred$fit - pred$se.fit * 1.96, col="grey80")
lines(18:80, pred$fit + pred$se.fit * 1.96, col="grey80")


# Lab Work II:
#----------------------------------------------------------------------
# Now, what is the difference in probabilites over age, regarding gender?

# And, does it matter if they read the nyt? How certain are you?



# Marginal Effects
#################################################################

# the marginal effect, d/dx, of choice depends on... you.

# 1) d/dE[x] - marginal effect of the averaged observation
# 2) E[d/dx] - Average marginal effect in your sample
# 3) d/dx*   - marginal effect of a particular unit of choice
# 4) d/dx1-d/dx2 - difference in marginal effect, a la interaction with binary


# average marginal effect of college
xb <- X %*% probit$coefficients
marg <- dnorm(xb) * probit$coefficients["college"]
mean(marg); sd(marg)
boxplot(marg)


# marginal effect of college for average individual
xb <- colMeans(X) %*% probit$coefficients
# Stop. why is colMeans improper? correct it. 
marg <- dnorm(xb) * probit$coefficients["college"]
marg
# how would you get standard errors?


# marginal effect of college for female age=25 nytimes=0
xb <- c(1, 25, 0, 0, median(db$college), 0) %*% probit$coefficients
marg <- dnorm(xb) * probit$coefficients["college"]
marg


