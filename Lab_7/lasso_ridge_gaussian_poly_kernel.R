#################################################################
# penalized likelihood (Lasso, Ridge, Gaussian and poly kernels.)
# Lab 7, 20.10.14
# Quant III, Elad Zippory
#################################################################


#----------------------------------------------------------------
# just like the curse of dimensionality,
# there is the curse of machine learning implementations.
# go for reputation above all. caret is made by Pfizer...
#----------------------------------------------------------------

#----------------------------------------------------------------
# The caret package is like a big wrapper that gives us
# useful utilities for a very large set of predictive models 
# from various packages.

# the list of the models supported is available here:
# http://topepo.github.io/caret/modelList.html
# caret support many many models. let's look on their families. 
# http://topepo.github.io/caret/similarity.html


# caret got a very nice website:
# http://topepo.github.io/caret/index.html
# and intro
# https://www.youtube.com/watch?v=7Jbb2ItbTC4
#----------------------------------------------------------------

# install.packages(c("caret","doParallel"))
rm(list=ls())
library(caret)

# we are going to do some heavy lifting
library(foreach)
library(doParallel)
cl<-makeCluster(3)
registerDoParallel(cl)

getDoParRegistered()
getDoParWorkers()

load(file = "disc2012_w_order.rda")

model=as.formula(log(budget)~
                   CCES_region+CCES_housing+NEW_race+CCES_educ+fam_size+
                   log(bar_gross_income)+martial_state+ref_age*fam_size+
                   CCES_housing*log(bar_gross_income)+
                   CCES_region*log(bar_gross_income)+
                   fam_size*log(bar_gross_income)+
                   I(fam_size^2)+I(log(bar_gross_income)^2)+
                   I(log(bar_gross_income)^3)+msa*log(bar_gross_income))

db$bar_gross_income[which(db$bar_gross_income==0)]=NA
db=na.omit(db)

# they have an internal function to training and test.
# it is not identical to sample() because of an internal treatment
# to ensure that the training and test have the same frequency of Y.
index_train=createDataPartition(y = db$budget,p = .5,list=F)
train=db[index_train,]
test=db[-index_train,]

fit_lasso=train(model,method = "lasso",data = train)
fit_lasso

#but let's say we care more for r^2
fit_lasso=train(model,method = "lasso",data = train,
                metric = "Rsquared",maximize = T)
fit_lasso

#caret default evaluate only 3 values per parameter, by default
fit_lasso=train(model,method = "lasso",data = train,
                metric = "Rsquared",maximize = T,tuneLength = 10)

fit_lasso
ggplot(fit_lasso) + theme(legend.position="top")


# stopCluster(cl)

# If we want to give train() more instructions on how to find the parameters,
# we are doing it via the trainControl function. The trainControl produces
# a list of control parameters. This is a bit more elegant then
# inputting the control list manually into train().
# this is REPEATED CV. which means the K-fold iteration is repeated r times (bootstrap).
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 3)
# fitControl is a utility object. it has nothing to do with our data.
fit_lasso_RCV=train(model,method = "lasso",data = train,
                    trControl = fitControl,metric = "Rsquared",
                    maximize = T,tuneLength = 10)
fit_lasso_RCV

ggplot(fit_lasso_RCV) + theme(legend.position="top")

# results are save in fit_lasso$finalModel
results_lasso=predict(fit_lasso,newdata = test)
RMSE(results_lasso,log(test$budget))
#oh baby. 


fit_ridge=train(model,method = "ridge",data = train)
results_ridge=predict(fit_ridge,newdata = test)
RMSE(results_ridge,log(test$budget))


# but this was just one paramerter. 
# what if we need to select two parameters, say lamda and sigma^2?
# one approch is a grid search:
# for each combination of the paramerters, we calculate the validation error
# 1. select folds 
# 2. for every combination, get mean validation error. 
# choose pair that optimizes your error matric

# the default for caret is 3 values per parameter. we might want more.


# let's have a more naive model
model=as.formula(budget~CCES_housing+fam_size+bar_gross_income+ref_age+msa)

# and for time constraints, let's cut the data.
index_train=createDataPartition(y = db$budget,p = .1,list=F)
train=db[index_train,]
test=db[-index_train,]

# A polynomial Kernel:
#fit_kernel_poly=train(model,method = "krlsPoly",data = train)

# A Gaussian Kernel:
#fit_kernel=train(model,method = "krlsRadial",data = train)

load(file = "kernel_results.rda")

fit_kernel_poly

fit_kernel

grid=expand.grid(degree=c(1,2,3),lambda=c(1,5,10,15))
fit_kernelgrid=train(
  model,method = "krlsPoly",data = train,tuneGrid = grid)


ggplot(fit_kernelgrid) + theme(legend.position="top")

# fit_kernel_poly=train(model,method = "krlsPoly",
#                       preProcess = c("center","scale"),
#                       data = train)
# preProcess is very flexible.
# preProcess() - an internal engine to process the data. center, scale, 
# impute missing values, pca or box - cox transformation etc etc
# variables we want to scale
# numerics=c("x1","x2","x3")
# ProcessCall=preProcess(churnTrain[,numerics],method=c("center","scale"))
# riddle - why are we using the data in the creation of the call?

# PS: kernel methods are a HUGE body of research.
#----------------------------------------

