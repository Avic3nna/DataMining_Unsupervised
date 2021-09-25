# Implementation of EM algorithm

# clear everything and load required libraries/codes
rm(list=ls())
library(mvtnorm)
library(car)
library(scales)
#source("C:/Users/Sven/Google Drive/Teaching/Data_Mining_2020/Lab3/mySum.R")
source("/Users/sven/Google Drive/Teaching/Data_Mining_2020/Lab3/mySum.R")


#loading and plotting data
#setwd("C:/Users/Sven/Google Drive/Teaching/Data_Mining_2020/Lab3/Data")
setwd("/Users/sven/Google Drive/Teaching/Data_Mining_2020/Lab3/Data")

#load("2dData-strange.RData")
#load("C:/Users/Sven/Google Drive/Teaching/Data_Mining_2020/Lab3/Data/2gaussiandata.RData")
load("/Users/sven/Google Drive/Teaching/Data_Mining_2020/Lab3/Data/2gaussiandata.RData")

plot(x[,1],x[,1], col=scales::alpha(x[,3],0.3), pch=20, xlim=c(-12,10),ylim=c(-12,12)) #*****plotting data

# data manipulation
classes <- x[,3] #labels vector
x <-x[,1:2] #removing labels from data
hist(x, col="blue")
plot(density(x))


#Step 1: initialization
pi1<-1.5  # 0.5
pi2<-0.5

#initial values for means
mu1<-matrix(c(sample(-10:0, 1),sample(-10:0, 1)),nrow=2) #random uniformly selected starting values
mu2<-matrix(c(sample(0:10, 1),sample(0:10, 1)),nrow=2) #random uniformly selected starting values

#initial values for covariance matrix for each cluster
#NB!! covariance matrix of sigmas, must be symmetric!!

sym1 = sample(-3:3, 1)
sym2 = sample(-3:3, 1)
sigma1<-matrix(c(sample(1:5, 1),sym1,sym1,sample(1:5, 1)),nrow=2)
sigma2<-matrix(c(sample(1:5, 1),sym2,sym2,sample(1:5, 1)),nrow=2)

#Plotting initizations in the data set
plot(x[,1],x[,2], col=scales::alpha(classes,0.3), pch=20, xlim=c(-12,10),ylim=c(-12,12)) #plotting data
par(new=TRUE) #to include the previous plot on the previous = combine plots

points(mu1[1], mu1[2], pch=18, cex=1, col="steelblue")
points(mu2[1], mu2[2], pch=18, cex=1, col="yellowgreen")

car::ellipse(center=as.vector(mu1),shape=sigma1, radius=3, col="steelblue")
car::ellipse(center=as.vector(mu2),shape=sigma2, radius=3, col="yellowgreen")

#initial conditions for stopping the algo
loglik<- rep(NA, 2000) #log likelihoods storage
loglik[1]<-0 #initial log likelihood value
loglik[2]<-mySum(pi1*(log(pi1)+log(matrix(dmvnorm(x,mu1,sigma1),nrow=2,ncol=1000))))+mySum(pi2*(log(pi2)+log(matrix(dmvnorm(x,mu2,sigma2),nrow=2,ncol=1000))))
k<-2

#main loop with step 2, 3 - EM
while(abs(loglik[k]-loglik[k-1]) >= 0.00001) {  #if no significant improvement, finish
  
  # Step 2 -> E-step: Expectation - Calculating the "Soft Labels" of Each Data Point
  tau1<-pi1*matrix(dmvnorm(x,mu1,sigma1))
  tau2<-pi2*matrix(dmvnorm(x,mu2,sigma2))

  normalizer<-tau1 + tau2
  
  
  tau1<-tau1/normalizer
  tau2<-tau2/normalizer
  
  # Step 3 -> M step: Maximization - Re-estimate the Component Parameters
  n<-dim(x)[1] #number of datapoints
  
  pi1<-mySum(tau1)/n #recomputing responsabilities
  #****missing 1 line here to implement pi2<-mySum(tau2)/n

  mu1[1]<-(t(tau1)%*%x[,1])/mySum(tau1) #recalculating mean values
  mu1[2]<-(t(tau1)%*%x[,2])/mySum(tau1)  #t(tau) to perform matrix multiplication, row by vector == scalar
  
  #****missing 2 lines here to implement mu2
  mu2[1]<-(t(tau2)%*%x[,1])/mySum(tau2)
  mu2[2]<-(t(tau2)%*%x[,2])/mySum(tau2)
  
  sigma1[1,1]<-t(tau1)%*%((x[,1]-mu1[1])*(x[,1]-mu1[1]))/(mySum(tau1)) #recalculating covariance matrix
  sigma1[1,2]<-t(tau1)%*%((x[,1]-mu1[1])*(x[,2]-mu1[2]))/(mySum(tau1))
  sigma1[2,1]<-sigma1[1,2]
  sigma1[2,2]<-t(tau1)%*%((x[,2]-mu1[2])*(x[,2]-mu1[2]))/(mySum(tau1))
  
  sigma2[1,1]<-t(tau2)%*%((x[,1]-mu2[1])*(x[,1]-mu2[1]))/(mySum(tau2))
  sigma2[1,2]<-t(tau2)%*%((x[,1]-mu2[1])*(x[,2]-mu2[2]))/(mySum(tau2))
  sigma2[2,1]<-sigma2[1,2]
  sigma2[2,2]<-t(tau2)%*%((x[,2]-mu2[2])*(x[,2]-mu2[2]))/(mySum(tau2))
  
  dev.off() #new graph plot
  plot(x[,1],x[,2], col=scales::alpha(classes,0.3), pch=20, xlim=c(-12,10),ylim=c(-12,12)) #plotting data
  points(mu1[1], mu1[2], pch=18, cex=1, col="steelblue")
  points(mu2[1], mu2[2], pch=18, cex=1, col="yellowgreen")
  
  car::ellipse(center=as.vector(mu1),shape=sigma1, radius=3, col="steelblue")
  car::ellipse(center=as.vector(mu2),shape=sigma2, radius=3, col="yellowgreen")
  
  #new loglik calculation
  loglik[k+1]<-mySum(pi1*(log(pi1)+log(matrix(dmvnorm(x,mu1,sigma1), nrow=2,ncol=1000))))
  
  k<-k+1
}


# Comparison with library implementation of EM
# library(mixtools)
# gm<-mvnormalmixEM(x,k=2,epsilon=1e-04)  #multivariate normal distribution EM
#                                         #normalmixEM is only for univariate normal
# gm$lambda
# gm$mu
# gm$sigma
# gm$loglik
# plot(gm, which=2)
# 
# (head(gm$posterior))
# pred<-apply(gm$posterior, 1, function(row) which.max(row))
# (confusionMatrix[1,1]+confusionMatrix[2,2])/1000 # accuracy
# (confusionMatrix[1,2]+confusionMatrix[2,1])/1000 # error rate
