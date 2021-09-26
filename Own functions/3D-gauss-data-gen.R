# Omar El Nahhas
# 18/09/2021
# Data Mining ITI8730
# Generate 3D dataset from Gaussian distribution

rm(list = ls())

library(mvtnorm) #multivariate normal distribution library
library(plot3D)
set.seed(420)

multi_gauss_data_gen = function(n, mu, covar){
  if(length(mu) != length(covar)){ #requires equal dim. 
    return(NaN)
  }
  
  #matrix frame that will contain all the data
  output = matrix(data=NA,n*length(mu),length(mu[[1]])+1)
  
  for(j in seq(along = 1:length(mu))){
    #returns n*length(mu[[j]]) coords
    mvn = rmvnorm(n=n, mean=mu[[j]], sigma=covar[[j]])
    
    #insert the generated data inside the x matrix and add label
    for (i in seq(along=1:n)){
      output[(j-1)*n+i,1:length(mu[[j]])] = mvn[i,]
      output[(j-1)*n+i,length(mu[[j]])+1] = j
    }
  }
  return(output)
}


### CONFIGURATION OF GAUSSIAN
n = 600 #samples per cluster

# mu_3D = list(c(2,0,2),  #mu1
#              c(1,10,1), #mu2
#              c(10,7,10), #mu3
#              c(11,0,11)) #mu4
# #...
# #random samples per matrix
# xy = sample(-4:4, 1)
# xz = sample(-4:4, 1)
# yz = sample(-4:4, 1)
# x = sample(1:5, 1)
# y = sample(1:5, 1)
# z = sample(1:5, 1)
# 
# xy1 = sample(-4:4, 1)
# xz1 = sample(-4:4, 1)
# yz1 = sample(-4:4, 1)
# x1 = sample(1:5, 1)
# y1 = sample(1:5, 1)
# z1 = sample(1:5, 1)
# 
# xy2 = sample(-4:4, 1)
# xz2 = sample(-4:4, 1)
# yz2 = sample(-4:4, 1)
# x2 = sample(1:5, 1)
# y2 = sample(1:5, 1)
# z2 = sample(1:5, 1)
# 
# xy3 = sample(-4:4, 1)
# xz3 = sample(-4:4, 1)
# yz3 = sample(-4:4, 1)
# x3 = sample(1:5, 1)
# y3 = sample(1:5, 1)
# z3 = sample(1:5, 1)
# #  covar  x xy xz 
# #         xy y yz 
# #         xz yz z
# 
# #needs to be positive semi-definite
# covar = list(matrix(c(x, xy,  xz,  xy, y,  yz,  xz,  yz,  z), ncol=length(mu_3D[[1]])),  #sigma1
#              matrix(c(x1,xy1, xz1, xy1,y1, yz1, xz1, yz1, z1),ncol=length(mu_3D[[1]])),  #sigma2
#              matrix(c(x2,xy2, xz2, xy2,y2, yz2, xz2, yz2, z2),ncol=length(mu_3D[[1]])),  #sigma3
#              matrix(c(x3,xy3, xz3, xy3,y3, yz3, xz3, yz3, z3),ncol=length(mu_3D[[1]])))  #sigma4
#...

#initial values for means
mu_3D = list(c(5*runif(1, min = -2, max = 0), runif(1, min = -2, max = 0), runif(1, min = -2, max = 0)),  #mu1
             c(3*runif(1, min = 0, max = 3), runif(1, min = 0, max = 3), runif(1, min = 0, max = 3)), #mu2
             c(0.5*runif(1, min = 3, max = 8), runif(1, min = 3, max = 8), runif(1, min = 3, max = 8)), #mu3
             c(5*runif(1, min = -7, max = -2), runif(1, min = -7, max = -2), runif(1, min = -7, max = -2))) #mu4
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

covar = list(sigma1,sigma2,sigma3,sigma4)

### END CONFIGURATION OF GAUSSIAN

#generation of gaussians, returns NaN if input dimensions don't match
generated_data = multi_gauss_data_gen(n, mu_3D, covar)

#plot clusters
library(rgl) #interactive
#remove x11() if using MacOS
plot3d(generated_data[,1], generated_data[,2], generated_data[,3], col = (generated_data[,ncol(generated_data)]), xlab = "x", ylab = "y", zlab = "z",surface = FALSE)

#remove labels
#no_label_data = generated_data[,-ncol(generated_data)]

#save(generated_data,file="./Own functions/data/3Dgauss.RData")
