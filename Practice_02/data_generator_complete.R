rm(list=ls())

# let us generate data
labelsNr<-3 #number of labels
labelsJ<-1:1:labelsNr #labels are numbers 1,2,3
labelsI<-1:1:500 #number of data points for label 500

#When the number of dimensions equals 2 -> bivariate normal distribution 
#that is defined with a two-dimensional vector of means, 
#m=(m1,m2) and covariance matrix sigma  ??=[(??1)^2, ??12, ??21, (??2)^2].
#reference: https://www.itl.nist.gov/div898/handbook/pmc/section5/pmc542.htm

sigma1<-matrix(c(4,2,2,3),ncol=2) #covariance matrix of sigmas, must be symmetric!!
sigma1
sigma2<-matrix(c(2,-2,-2,6),ncol=2)
sigma3<-matrix(c(4,-4,-4,5),ncol=2)

# more ellegant way to construct 
eig_vec_1 <- matrix(c(0.4,0.2,0.2,0.4), nrow=2, ncol=2)
eig_vec_2 <- matrix(c(0.2,-0.1,-0.1,0.2), nrow=2, ncol=2)

eig_val_1<- matrix(c(1,0,0,1) ,nrow=2, ncol=2)
eig_val_2<- matrix(c(1,0,0,1) ,nrow=2, ncol=2)

#covariance matrices
#sigma1 <- eig_vec_1 %*% eig_val_1 %*% t(eig_vec_1)
#sigma2 <-  eig_vec_2 %*% eig_val_2 %*% t(eig_vec_2)

mean1=c(2,0) #mean of both 2 variables/dimensions
mean1
mean2=c(1,10)
mean3=c(10,7)
#Sigmas=data.frame(sigma1,sigma2,sigma3)
Sigmas=list(sigma1,sigma2,sigma3)
Sigmas
Means=list(mean1,mean2,mean3)
x<-matrix(,1500,3)  #matrix that will contain all the data

library(mvtnorm) #multivariate normal distribution library (multivariate - more than 1 dimension, variable)

for (j in seq(along=labelsJ)){
  sigma=Sigmas[[j]]
  mean=Means[[j]]
  xx <- rmvnorm(n=500, mean=mean, sigma=sigma) #random multivariate normal distribution function
  a<-switch(j,"red","green","blue"); #dot color
  plot(xx,col=a,type="p",xlim=c(-10,20),ylim=c(-10,20)) #plot xx
  par(new=TRUE) #to include the previous plot on the previous = combine plots
  for (i in seq(along=labelsI)){ #insert the generated data inside the x matrix and add label
   x[(j-1)*500+i,1:2]=xx[i,]
   x[(j-1)*500+i,3]=j
  }
}

x <- x[,1:2] # the data we used was initially prepared for the classification example please remove third column 

#save(x,file="/Users/sven/Google Drive/Teaching/Data_Mining_2021/Practice_02/kdata.RData")


