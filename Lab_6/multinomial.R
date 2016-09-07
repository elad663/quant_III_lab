#################################################################
# Multinomial
# Lab 6, 13.10.14
# Quant III, Elad Zippory
#################################################################

rm(list=ls())
set.seed(123)

library(MASS)
library(nnet)
library(ggplot2)
library(reshape2)

load(file = "united_fmly_2012.rda")

# I usually don't use attach(). be mindfull of it. R will not protect you.
attach(fmly2012)
# 1 Married
# 2 Widowed
# 3 Divorced
# 4 Separated
# 5 Never married
table(marital1)


fmly2012$sex_ref=as.numeric(sex_ref)
form=as.formula(marital1~sex_ref+age_ref)


model=multinom(formula = form,data = fmly2012,Hess = T,maxit=1000)

summary(model)
summed_model=summary(model)
dim(summed_model$coefficients)
dim(summed_model$standard.errors)


# pretty trivial to get predicted probabilites:
# by category:
par(mfrow=c(1,2))
newdata_1=data.frame(sex_ref=1,age_ref=median(age_ref))
plot(x = 1:length(unique(marital1)),
     y=predict(object = model,newdata = newdata_1,type="probs"),
     type="h",ylab = "pie",xlab="1",ylim=c(0,.7))

newdata_2=data.frame(sex_ref=2,age_ref=median(age_ref))
plot(x = 1:length(unique(marital1)),
     y=predict(object = model,newdata = newdata_2,type="probs"),
     type="h",ylab = "pie",xlab="2",ylim=c(0,.7))
par(mfrow=c(1,1))

# by age and gender, with ggplot
newdata <- rbind(data.frame(sex_ref=1,age_ref=min(age_ref):max(age_ref)),
                 data.frame(sex_ref=0,age_ref=min(age_ref):max(age_ref)))
pred <- predict(object = model,newdata = newdata,type="probs")

data_melted <- melt(data=as.data.frame(cbind(newdata,pred)),measure.vars=3:7)

ggplot(data=data_melted, aes(x=age_ref, y=value, group = variable, colour = as.factor(variable))) +
    geom_line() + ylim(0, 1) + facet_wrap(~sex_ref)

# OH MY GOD PEOPLE START TO DIE AS THEY GET OLD AND MEN DIE FIRST! 

# but how certain are we?

# reminder...
matrix(sqrt(diag(solve(model$Hessian))),nrow = nrow(summed_model$standard.errors),byrow = T)
summed_model$standard.errors

# varcov matrix for simulations:
H=solve(model$Hessian)

ITER=10000
sim_mat=mvrnorm(n=ITER, mu=as.numeric(summed_model$coefficients),Sigma=H)
cbind(colMeans(sim_mat),as.numeric(summed_model$coefficients))

get_prob_multinomial <- function(betas,x,levs)
{
    # intercept..
    x <- as.matrix(cbind(1,x))
    # so we have one treatment for all categories, add zero:
    betas <- as.matrix(rbind(0,matrix(data = betas,nrow = levs)))
    exp(x%*%t(betas))/sum(exp(x%*%t(betas)))
}

predict(object = model,newdata = newdata_1,type="probs")
get_prob_multinomial(betas = as.numeric(summed_model$coefficients),levs=4,x=newdata_1)

ans <- apply(sim_mat, 1,get_prob_multinomial,levs=4,x=newdata_1)
round(rowMeans(ans),10)

# how would you explain this?

# Lab task: use get_prob_multinomial() to simulated standard errors by age\gender


