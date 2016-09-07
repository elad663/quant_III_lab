#################################################################
# MLE for ordinal probit. Constrained optimization. Marginal effects.
# Lab 5, 6.10.14
# Quant III, Elad Zippory
#################################################################

rm(list=ls())
set.seed(123)

# we assume the proportional odds ratios model.
# Ordered logit is a constrained optimization
#  From Greene E7 p.788:
# P(Y=0) = \Phi(-Inf -xb)
# P(Y=1) = \Phi(c1-xb)-\Phi(-xb)
# P(Y=2) = \Phi(c2-xb)-\Phi(c1-xb)
# P(Y=J) = \Phi(Inf-xb)-\Phi(c_{j-1}-xb)
# So the costraints are: c1<c2<..<c_{j-1}

# as discussed in class, we cannot estimate all the cuntpoints AND a constant.
# note that Greene assumes that c0 is zero, this allow to estimate the constant.

# Plan of lab:
# 1. write two constrained likelihood functions, with or without a constant, for a given number of categories.
# 2. see what compares to the recommended function- MASS:::polr
# 3. upgrade our function to get an arbitrary number of categories
# 4. add marginal effects!
# 5. Lab work: relax the proportional odds ratios assumption, to the cumulative logit model.

#################################################################
# 1. constrained likelihood
#################################################################
# constrOptim() minimizes a function subject to linear inequality constraint.
# so we need to state the constraints matrix. 
# let's look at the help file
?constrOptim

# if 3 cutpoints: note that what we will pass into constrOptim will be
# a bit different as we will need to include zeros for the betas.
u_i_no_constant <- rbind(c(1,0,0),
                         c(-1,1,0),
                         c(0,-1,1))
c_i_no_constant <- numeric(nrow(u_i_no_constant))

# one less constraint!
u_i_yes_constant <- rbind(c( 1,0),
                          c(-1,1))
c_i_yes_constant <- numeric(nrow(u_i_yes_constant))
#################################################################

# manual loklikelihood ordinal probit for 4 categories.
# for arbitrary function see below
#################################################################
no_constant_lk <- function(params,x,y){
  
  J <- length(levels(y))-1
  cuts <- params[1:J]
  betas <- params[(J+1):length(params)]
  
  xb <- x%*%betas
  
  t1 <- cuts[1]
  t2 <- cuts[2]
  t3 <- cuts[3]

  p1 <- log(pnorm(t1-xb)-pnorm(-Inf-xb))
  p2 <- log(pnorm(t2-xb)-pnorm(t1-xb))
  p3 <- log(pnorm(t3-xb)-pnorm(t2-xb))
  p4 <- log(pnorm(Inf-xb)-pnorm(t3-xb))
  
  -sum(cbind(y==1,y==2,y==3,y==4) * cbind(p1,p2,p3,p4))
}
#################################################################

yes_constant_lk <- function(params,x,y){
    
    J <- length(levels(y))-2
    cuts <- params[1:J]
    betas <- params[(J+1):length(params)]
    
    xb <- x%*%betas
    
    t1 <- cuts[1]
    t2 <- cuts[2]
    
    p1 <- log(pnorm(0-xb)-pnorm(-Inf-xb))
    p2 <- log(pnorm(t1-xb)-pnorm(0-xb))
    p3 <- log(pnorm(t2-xb)-pnorm(t1-xb))
    p4 <- log(pnorm(Inf-xb)-pnorm(t2-xb))
    
    -sum(cbind(y==1,y==2,y==3,y==4) * cbind(p1,p2,p3,p4))
}
#################################################################

data <- read.csv("lab5_data.csv", stringsAsFactors=F)

Y <- data$nytimes
X_yes <- cbind(1,data$age, data$female, data$high.school, data$college)
X_no <- cbind(data$age, data$female, data$high.school, data$college)

# let's update the dimensions of the constraints matrix
u_i_no_constant <- cbind(u_i_no_constant,matrix(0,ncol = ncol(X_no),
                                                nrow = nrow(u_i_no_constant)))

u_i_yes_constant <- cbind(u_i_yes_constant,matrix(0,ncol = ncol(X_yes),
                                                nrow = nrow(u_i_yes_constant)))

# The initial values must be feasible.
# use ls estimates as starting values
q1 <- length(unique(Y))%/%2L
y1 <- (Y > q1)

stval_yes_constant <- c(1:(length(unique(Y))-2),
           glm.fit(X_yes, y1, family = binomial("probit"))$coefficients)

stval_no_constant <- c(1:(length(unique(Y))-1),
                        glm.fit(X_no, y1, family = binomial("probit"))$coefficients)
Y <- as.factor(Y)

#################################################################
# Nelder-Mead is our optimization algorithm. 
oprobit_fit_no <- constrOptim(theta = stval_no_constant, no_constant_lk, grad = NULL,
                           ui = u_i_no_constant,ci = c_i_no_constant,
                           method="Nelder-Mead", x=X_no, y=Y)

oprobit_fit_yes <- constrOptim(theta = stval_yes_constant, yes_constant_lk, grad = NULL,
                                ui = u_i_yes_constant,ci = c_i_yes_constant,
                                method="Nelder-Mead", x=X_yes, y=Y)
#################################################################

#################################################################
# 2. let's compare with the recommended R package for this task
# if you want priors, use arm:::bayespolr
# bayespolr(formula = factor(nytimes,ordered = T) ~ age + female + high.school + college,
#           data = data,method == "probit")
#################################################################

oprobit_fit_no$par
oprobit_fit_yes$par
# first, the betas are basically identical.

library(MASS)
oprobit <- polr(factor(nytimes,ordered = T) ~ age + female + high.school + college, 
  data=data, method="probit")
summary(oprobit) # note - polr uses optim with BFGS, which explains some small differences.

# second - polr gives the same estiamtes as the manual model without a constant,
# in contrary to their documentation... "The model must have an intercept"

#################################################################

#################################################################
# 3. Now, let's add support for arbitrary number of categories.
# It should be straightforward how to move from the proportional odd ratios
# model to the cumulative logit
#################################################################
ordered_lk <- function(params,x,y){
    
    J <- length(levels(y))-1
    cuts <- params[1:J]
    betas <- params[(J+1):length(params)]
    xb <- x%*%betas
    xb <- xb[,1] # don't be a matrix. respect dimensions.
    
    cuts <- c(-Inf,cuts,Inf)
    cuts_mat <- matrix(data = cuts,nrow = length(xb),ncol = length(cuts),byrow = T)
    
    inner_mat <- pnorm(cuts_mat-xb)
    
    log_mat <- log(inner_mat[,2:(ncol(inner_mat))]-inner_mat[,1:(ncol(inner_mat)-1)])
    
    boolean_mat <- y==matrix(1:length(levels(y)),nrow = length(y),ncol=length(levels(y)),byrow = T)

    -sum(log_mat * boolean_mat)
}

get_constraints <- function(y,x){
    u_i <- matrix(0,nrow=length(levels(y))-2,ncol=length(levels(y))-2)
    diag(u_i) <- -1
    u_i <- cbind(rbind(numeric(nrow(u_i)),u_i),numeric(nrow(u_i)+1))
    diag(u_i) <- 1
    u_i <- cbind(u_i,matrix(0,ncol = ncol(x),nrow = nrow(u_i)))
    c_i <- numeric(nrow(u_i))
    return(list(u_i=u_i,c_i=c_i))
}

constraints <- get_constraints(x=X_no, y=Y)

oprobit_fit_gen <- constrOptim(theta = stval_no_constant, ordered_lk, grad = NULL,
                              ui = constraints$u_i,ci = constraints$c_i,
                              method="Nelder-Mead", x=X_no, y=Y)

oprobit_fit_no$par
oprobit_fit_gen$par
# and everybody were happy. (not really, as the generic function is less efficient)
#################################################################

#################################################################
# 4. marginal effects for ordered logit
me_ordered <- function(model){
    
    #######################!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # WARNING!!! you are going to get here the marginal effects per column.
    # if you got non-linearities, or interactions, two options:
    # 1. take the proper derivative as done on the whiteboard
    # 2. use the absolute difference function to obviate it altogether.
    #######################!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    betas <- model$coefficients
    zetas <- model$zeta
    
    X <- model.matrix(model$terms,data = model$model)[,-1]
    
    XB <- (as.matrix(X)%*%betas)[,1]
    
    cuts <- matrix(data = c(-Inf,zetas,-Inf), # two -Inf becuase the 1 drops
                   nrow = length(XB),
                   ncol = (length(zetas)+2),byrow = T)
    
    sub_mat <- cuts-XB
    
    subtraction <- dlogis(sub_mat[,1:(ncol(sub_mat)-1)])-dlogis(sub_mat[,2:(ncol(sub_mat))])

    return(t(sapply(betas, function(beta) {
        ME <- beta*subtraction
        ans <- colMeans(ME)
        names(ans) <- levels(model$model[,1])
        return(ans)        
    })))
}

ologit <- polr(factor(nytimes,ordered = T) ~ age + female + high.school + college, 
               data=data, method="logistic")

me_ordered(model=ologit)

# how would you estimate standard errors for it? this should become old.
# how would you add support to get marginal effects of a particular X?
#################################################################

#################################################################
# 5. Lab work: 1. write the likelihood of the cumulative logit model.
#              2. test the assumptions of the proportional odds ratios for this data



