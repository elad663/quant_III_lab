#################################################################
# pca and k-means
# Lab 7, 20.10.14
# Quant III, Elad Zippory
#################################################################

rm(list=ls())
library("lattice")
load(file = "pca_kmeans.rda")

X=as.matrix(std_fulldf)

Sigma=cor(X)
V=eigen(Sigma)[[2]]

W=X%*%V

round(eigen(Sigma)[[1]],2)

xyplot(W[,2]~W[,1],group=foodclass,auto.key=T)

clusters=kmeans(X,centers=4)

xyplot(W[,2]~W[,1],group=clusters$cluster,auto.key=T)