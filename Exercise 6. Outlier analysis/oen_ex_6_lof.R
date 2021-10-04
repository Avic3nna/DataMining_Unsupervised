rm(list=ls())
set.seed(1337)

packages_used = c("rstudioapi",
                  "mvtnorm",  
                  "car",
                  "scales")

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

library(mvtnorm)
library(car)
library(scales)

source("./Exercise 1. Distance function/oen_minkowski.R")

load("./Data/3Dgauss.RData")

no_label_data = generated_data[,1:3]



### compute reachability distance

#define how many neighbours to look at
k_nn = 3

dist = array(NA, dim(no_label_data)[1])
V_k_x = array(NA, dim(no_label_data)[1])
knn_radius_points = matrix(0, nrow=dim(no_label_data)[1], ncol=k_nn)

for(x in seq(along = 1:dim(no_label_data)[1])){
  dist = array(NA, dim(no_label_data)[1])
  for(y in seq(along = 1:dim(no_label_data)[1]))
  {
    dist[y] = minkowsky(no_label_data[x,], no_label_data[y,], 2)
  }
  sorted_dist = sort(dist, index.return=TRUE) #sort increasing

  V_k_x[x] = sorted_dist$x[k_nn+1] #k_nn+1 to skip itself # get the max distance (k-nn != possible points in radius)
  
  all_points = sorted_dist$x[sorted_dist$x <= V_k_x[x]] # get all points in the knn radius
  
  L_k_x = length(knn_radius_points) # amount of points in radius
  knn_radius_points[x,] = sorted_dist$ix[2:(length(all_points))][1:ncol(knn_radius_points)]
}


A_R_k_x = array(0, dim(no_label_data)[1])
  
  #reachability distance
for(i in seq(along=1:dim(no_label_data)[1])){ #gets the datapoint in the neighborhood
  for(j in seq(along= 1:length(knn_radius_points[i,]))){
    x = minkowsky(no_label_data[i,], no_label_data[knn_radius_points[i,j],], 2)
    y = V_k_x[knn_radius_points[i,j]]
    A_R_k_x[i] = A_R_k_x[i] + max(x,y)
  }
  A_R_k_x[i] = A_R_k_x[i] / length(knn_radius_points[i,])
}

LOF = matrix(0,nrow = nrow(no_label_data))
for (i in seq(along=1:nrow(no_label_data))){
  for (j in seq(along=1:length(knn_radius_points[i,]))){
    LOF[i] <- LOF[i] + A_R_k_x[i] / A_R_k_x[knn_radius_points[i,j]]
  }
  LOF[i] <- LOF[i] / ncol(knn_radius_points)
}


plot(density(LOF))
abline(v=2, col='red')



n = dim(generated_data)[1]

outliers = order(LOF, decreasing=T)[1:13]

pch <- rep('.', n)

pch[outliers] <- "x"

col <- rep("black", n)

col[outliers] <- "red"

cex <- rep(3, n)
cex[outliers] <- 4

generated_data = as.data.frame(generated_data)
colnames(generated_data) = c('x','y','z','label')


pairs(generated_data[,1:3], pch=pch, cex=cex,col=col)