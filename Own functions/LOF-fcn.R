rm(list=ls())
library(mvtnorm)
library(car)
library(scales)
set.seed(1337)




root_path = "G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/data-mining-iti8730"

setwd(root_path)

source("./Own functions/minkowsky-dist.R")
load("./practice_3/Data/JGdata.RData")

no_label_data = x[,1:2]

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
    s_x_y = minkowsky(no_label_data[i,], no_label_data[knn_radius_points[i,j],], 2)
    v_k_y = V_k_x[knn_radius_points[i,j]]
    max_dist = max(s_x_y,v_k_y)
    A_R_k_x[i] = A_R_k_x[i] + max_dist
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


LOF=sort(LOF,decreasing = TRUE)

# R implementation
library(DDoutlier)
lof <- DDoutlier::LOF(no_label_data, k = 3)

lof=sort(lof,decreasing = TRUE)



############### ERROR

mse = sum((LOF - lof)**2)/length(lof)
