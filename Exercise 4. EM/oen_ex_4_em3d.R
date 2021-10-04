rm(list=ls())
set.seed(1337)

packages_used = c("rstudioapi",
                  "mvtnorm",   
                  "car",
                  "scales",
                  "rgl") # #interactive 3D plot

for(package in packages_used){
  if(package %in% rownames(installed.packages()) == FALSE) {
    install.packages(package)
  }
}

setwd_current_path = function(){
  library(rstudioapi)
  current_path = getActiveDocumentContext()$path
  setwd(dirname(current_path)) #get this current folder
  setwd('..') #go 1 up for scalability
  print(getwd())
}
setwd_current_path()


source("./Exercise 1. Distance function/oen_minkowski.R")
source("./Exercise 4. EM/mySum.R")

load("./Data/3Dgauss.RData")

x = generated_data
# clear everything and load required libraries/codes


library(mvtnorm)
library(car)
library(scales)
library(rgl)


# data manipulation
classes <- generated_data[,4] #labels vector
no_label_data <-generated_data[,1:3] #removing labels from data
hist(no_label_data, col="blue")
plot(density(no_label_data))


#Step 1: initialization
pi1<- 0.3 
pi2<- 0.15
pi3<- 0.1
pi4<- 0.2

#initial values for means
mu_3D = list(c(runif(1, min = -2, max = 0), runif(1, min = -2, max = 0), runif(1, min = -2, max = 0)),  #mu1
             c(runif(1, min = 0, max = 3), runif(1, min = 0, max = 3), runif(1, min = 0, max = 3)), #mu2
             c(runif(1, min = 3, max = 8), runif(1, min = 3, max = 8), runif(1, min = 3, max = 8)), #mu3
             c(runif(1, min = -7, max = -2), runif(1, min = -7, max = -2), runif(1, min = -7, max = -2))) #mu4
#...

xy = runif(1, min=-4, max=4)
xz = runif(1, min=-4, max=4)
yz = runif(1, min=-4, max=4)
x = runif(1, min=1, max=5)
y = runif(1, min=1, max=5)
z = runif(1, min=1, max=5)

xy1 = runif(1, min=-4, max=4)
xz1 = runif(1, min=-4, max=4)
yz1 = runif(1, min=-4, max=4)
x1 = runif(1, min=1, max=5)
y1 = runif(1, min=1, max=5)
z1 = runif(1, min=1, max=5)

xy2 = runif(1, min=-4, max=4)
xz2 = runif(1, min=-4, max=4)
yz2 = runif(1, min=-4, max=4)
x2 = runif(1, min=1, max=5)
y2 = runif(1, min=1, max=5)
z2 = runif(1, min=1, max=5)

xy3 = runif(1, min=-4, max=4)
xz3 = runif(1, min=-4, max=4)
yz3 = runif(1, min=-4, max=4)
x3 = runif(1, min=1, max=5)
y3 = runif(1, min=1, max=5)
z3 = runif(1, min=1, max=5)
#  covar  x xy xz 
#         xy y yz 
#         xz yz z

eig_vec_1 = matrix(c(x, xy,  xz,  xy, x,  yz,  xz,  yz,  x), ncol=length(mu_3D[[1]]))
eig_vec_2 = matrix(c(x1,xy1, xz1, xy1,x1, yz1, xz1, yz1, x1),ncol=length(mu_3D[[1]]))
eig_vec_3 = matrix(c(x2,xy2, xz2, xy2,x2, yz2, xz2, yz2, x2),ncol=length(mu_3D[[1]]))
eig_vec_4 = matrix(c(x3,xy3, xz3, xy3,x3, yz3, xz3, yz3, x3),ncol=length(mu_3D[[1]]))

eig_val_1 = matrix(c(1,0,0,0,1,0,0,0,1), nrow=length(mu_3D[[1]]))
eig_val_2 = matrix(c(1,0,0,0,1,0,0,0,1), nrow=length(mu_3D[[1]]))
eig_val_3 = matrix(c(1,0,0,0,1,0,0,0,1), nrow=length(mu_3D[[1]]))
eig_val_4 = matrix(c(1,0,0,0,1,0,0,0,1), nrow=length(mu_3D[[1]]))

# #needs to be positive semi-definite
# covar = list(matrix(c(x, xy,  xz,  xy, y,  yz,  xz,  yz,  z), ncol=length(mu_3D[[1]])),  #sigma1
#              matrix(c(x1,xy1, xz1, xy1,y1, yz1, xz1, yz1, z1),ncol=length(mu_3D[[1]])),  #sigma2
#              matrix(c(x2,xy2, xz2, xy2,y2, yz2, xz2, yz2, z2),ncol=length(mu_3D[[1]])),  #sigma3
#              matrix(c(x3,xy3, xz3, xy3,y3, yz3, xz3, yz3, z3),ncol=length(mu_3D[[1]])))  #sigma4
#...
#covariance matrices
sigma1 <- eig_vec_1 %*% eig_val_1 %*% t(eig_vec_1)
sigma2 <-  eig_vec_2 %*% eig_val_2 %*% t(eig_vec_2)
sigma3 <-  eig_vec_3 %*% eig_val_3 %*% t(eig_vec_3)
sigma4 <-  eig_vec_4 %*% eig_val_4 %*% t(eig_vec_4)


#Plotting initizations in the data set
plot3d(no_label_data[,1], no_label_data[,2], no_label_data[,3], type='p')
par(new=TRUE) #to include the previous plot on the previous = combine plots

points3d(mu_3D[[1]][1], mu_3D[[1]][2], mu_3D[[1]][3], pch=18, cex=1, col="blue")
points3d(mu_3D[[2]][1], mu_3D[[2]][2], mu_3D[[2]][3], pch=18, cex=1, col="green")
points3d(mu_3D[[3]][1], mu_3D[[3]][2], mu_3D[[3]][3], pch=18, cex=1, col="red")
points3d(mu_3D[[4]][1], mu_3D[[4]][2], mu_3D[[4]][3], pch=18, cex=1, col="purple")

plot3d(ellipse3d(sigma1, centre = mu_3D[[1]]), col = "green", alpha = 0.5, add = TRUE)
plot3d(ellipse3d(sigma2, centre = mu_3D[[2]]), col = "orange", alpha = 0.5, add = TRUE)
plot3d(ellipse3d(sigma3, centre = mu_3D[[3]]), col = "purple", alpha = 0.5, add = TRUE)
plot3d(ellipse3d(sigma4, centre = mu_3D[[4]]), col = "red", alpha = 0.5, add = TRUE)

#library(generalCorr)
#minor(x, row, column)


#initial conditions for stopping the algo
loglik<- rep(NA, 2000) #log likelihoods storage
loglik[1]<-0 #initial log likelihood value
loglik[2]<-mySum(pi1*(log(pi1)+log(matrix(dmvnorm(no_label_data,mu_3D[[1]],sigma1),nrow=4,ncol=600))))+
  mySum(pi2*(log(pi2)+log(matrix(dmvnorm(no_label_data,mu_3D[[2]],sigma2),nrow=4,ncol=600)))) +
  mySum(pi3*(log(pi3)+log(matrix(dmvnorm(no_label_data,mu_3D[[3]],sigma3),nrow=3,ncol=600)))) +
  mySum(pi4*(log(pi4)+log(matrix(dmvnorm(no_label_data,mu_3D[[4]],sigma4),nrow=3,ncol=600))))

k<-2



sigmas = list(sigma1,sigma2,sigma3,sigma4)


tau1<-pi1*matrix(dmvnorm(no_label_data,mu_3D[[1]],sigmas[[1]]))
tau2<-pi2*matrix(dmvnorm(no_label_data,mu_3D[[2]],sigmas[[2]]))
tau3<-pi3*matrix(dmvnorm(no_label_data,mu_3D[[3]],sigmas[[3]]))
tau4<-pi4*matrix(dmvnorm(no_label_data,mu_3D[[4]],sigmas[[4]]))
tau = list(tau1, tau2, tau3, tau4)

#main loop with step 2, 3 - EM
while(abs(loglik[k]-loglik[k-1]) >= 1e-3) {  #if no significant improvement, finish

  # Step 2 -> E-step: Expectation - Calculating the "Soft Labels" of Each Data Point
  
  tau[[1]]<-pi1*matrix(dmvnorm(no_label_data,mu_3D[[1]],sigmas[[1]]))
  tau[[2]]<-pi2*matrix(dmvnorm(no_label_data,mu_3D[[2]],sigmas[[2]]))
  tau[[3]]<-pi3*matrix(dmvnorm(no_label_data,mu_3D[[3]],sigmas[[3]]))
  tau[[4]]<-pi4*matrix(dmvnorm(no_label_data,mu_3D[[4]],sigmas[[4]]))

  normalizer<-tau[[1]] + tau[[2]] + tau[[3]] + tau[[4]]
  
  
  tau[[1]]<-tau[[1]]/normalizer
  tau[[2]]<-tau[[2]]/normalizer
  tau[[3]]<-tau[[3]]/normalizer
  tau[[4]]<-tau[[4]]/normalizer
  
  # Step 3 -> M step: Maximization - Re-estimate the Component Parameters
  n<-dim(no_label_data)[1] #number of datapoints
  
  pi1<-mySum(tau[[1]])/n #recomputing responsabilities
  pi2<-mySum(tau[[2]])/n
  pi3<-mySum(tau[[3]])/n
  pi4<-mySum(tau[[4]])/n

  
  for(i in seq(along=1:length(mu_3D))){
    for(x in seq(along=1:length(mu_3D[[1]]))){
      mu_3D[[i]][x] = (t(tau[[i]])%*%no_label_data[,x])/mySum(tau[[i]])
    }
  }


  #  covar  x xy xz 
  #         xy y yz 
  #         xz yz z
  
  #recompute first cov. matrix
  for(i in seq(along = 1:length(sigmas))){        #1:4 -> 4 clusters
    for(x in seq(along = 1:nrow(sigmas[[1]]))){   #1:3 -> 3D
      for(y in seq(along = 1:ncol(sigmas[[1]]))){ #1:3 -> 3D
        sigmas[[i]][x,y] = t(tau[[i]])%*%((no_label_data[,x]-mu_3D[[i]][x])*(no_label_data[,y]-mu_3D[[i]][y]))/(mySum(tau[[i]])) 
        #recalculating covariance matrix
      }
    }
  }

  plot3d(no_label_data[,1], no_label_data[,2], no_label_data[,3], xlab = "x", ylab = "y", zlab = "z", type='p')
  
  #if(dev.cur() > 1) { par(new=TRUE) }
  
  par(new=TRUE)
  points3d(mu_3D[[1]][1], mu_3D[[1]][2], mu_3D[[1]][3], size=18, cex=1, col="blue")
  points3d(mu_3D[[2]][1], mu_3D[[2]][2], mu_3D[[2]][3], size=18, cex=1, col="green")
  points3d(mu_3D[[3]][1], mu_3D[[3]][2], mu_3D[[3]][3], size=18, cex=1, col="red")
  points3d(mu_3D[[4]][1], mu_3D[[4]][2], mu_3D[[4]][3], size=18, cex=1, col="purple")
  
  plot3d(ellipse3d(sigmas[[1]], centre = as.vector(mu_3D[[1]])), col = "green", level = 0.95, add=TRUE, alpha=0.2)
  plot3d(ellipse3d(sigmas[[2]], centre = as.vector(mu_3D[[2]])), col = "orange", level = 0.95, add=TRUE, alpha=0.2)
  plot3d(ellipse3d(sigmas[[3]], centre = as.vector(mu_3D[[3]])), col = "purple", level = 0.95, add=TRUE, alpha=0.2)
  plot3d(ellipse3d(sigmas[[4]], centre = as.vector(mu_3D[[4]])), col = "red", level = 0.95, add=TRUE, alpha=0.2)
  
  #new loglik calculation
  loglik[k+1]<-
    mySum(pi1*(log(pi1)+log(matrix(dmvnorm(no_label_data,mu_3D[[1]],sigmas[[1]]),nrow=4,ncol=600)))) +
    mySum(pi2*(log(pi2)+log(matrix(dmvnorm(no_label_data,mu_3D[[2]],sigmas[[2]]),nrow=4,ncol=600)))) +
    mySum(pi3*(log(pi3)+log(matrix(dmvnorm(no_label_data,mu_3D[[3]],sigmas[[3]]),nrow=4,ncol=600)))) +
    mySum(pi4*(log(pi4)+log(matrix(dmvnorm(no_label_data,mu_3D[[4]],sigmas[[4]]),nrow=4,ncol=600))))
  
  k<-k+1
  #Sys.sleep(2) #visualise ellipse changes
}

#dev.off() #new graph plot
plot3d(no_label_data[,1], no_label_data[,2], no_label_data[,3], xlab = "x", ylab = "y", zlab = "z", type='p')

#if(dev.cur() > 1) { par(new=TRUE) }

points3d(mu_3D[[1]][1], mu_3D[[1]][2], mu_3D[[1]][3], size=18, cex=1, col="black")
points3d(mu_3D[[2]][1], mu_3D[[2]][2], mu_3D[[2]][3], size=18, cex=1, col="red")
points3d(mu_3D[[3]][1], mu_3D[[3]][2], mu_3D[[3]][3], size=18, cex=1, col="green")
points3d(mu_3D[[4]][1], mu_3D[[4]][2], mu_3D[[4]][3], size=18, cex=1, col="blue")

plot3d(ellipse3d(sigmas[[1]], centre = as.vector(mu_3D[[1]])), col = "green", level = 0.95, add=TRUE, alpha=0.2)
plot3d(ellipse3d(sigmas[[2]], centre = as.vector(mu_3D[[2]])), col = "orange", level = 0.95, add=TRUE, alpha=0.2)
plot3d(ellipse3d(sigmas[[3]], centre = as.vector(mu_3D[[3]])), col = "purple", level = 0.95, add=TRUE, alpha=0.2)
plot3d(ellipse3d(sigmas[[4]], centre = as.vector(mu_3D[[4]])), col = "red", level = 0.95, add=TRUE, alpha=0.2)

