rm(list=ls())
set.seed(1337)

packages_used = c("rstudioapi",
                  "mixtools",   # em
                  "shotGroups") # k-means

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

library(mixtools)
library(shotGroups)

source("./Exercise 1. Distance function/oen_minkowski.R")
source("./Exercise 2. Feature selection/oen_entropy.R")
source("./Exercise 2. Feature selection/oen_hopkins.R")
source("./Exercise 5. Cluster validation/oen_silhouette.R")

load("./Data/3Dgauss.RData")

x = generated_data



### without Z
feat_select = c(1,2,4) 
not_z_entr = entropy_2d(x[,feat_select])
not_z_hopk = hopkins_stat(x[,feat_select])

#EM

gm = mvnormalmixEM(x[,feat_select[1:2]], k=4, epsilon = 1e-4)
plot(gm, which=2)
labels_em=apply(gm$posterior, 1, function(row) which.max(row)) #labels
#apply silhouette coeff
new_data_em = cbind(x[,feat_select[1:2]], labels_em)
not_z_scoeff_em = silhouette_coeff(new_data_em)
print(not_z_scoeff_em)
print(mean(not_z_scoeff_em))

#K-means
k_out = kmeans(x[,feat_select[1:2]], 4)
labels_k = k_out$cluster
new_data_k = cbind(x[,feat_select[1:2]], labels_k)


plot(new_data_k[,1], new_data_k[,2], col=new_data_k[,3], xlab='x', ylab = 'y')

#apply silhouette coeff
not_z_scoeff_k = silhouette_coeff(new_data_k)
print(not_z_scoeff_k)
print(mean(not_z_scoeff_k))



print(not_z_entr)
print(not_z_hopk)





### without Y
feat_select = c(1,3,4)
not_y_entr = entropy_2d(x[,feat_select])
not_y_hopk = hopkins_stat(x[,feat_select])

#EM

gm = mvnormalmixEM(x[,feat_select[1:2]], k=4, epsilon = 1e-4)
plot(gm, which=2)
labels_em=apply(gm$posterior, 1, function(row) which.max(row)) #labels
#apply silhouette coeff
new_data_em = cbind(x[,feat_select[1:2]], labels_em)
not_y_scoeff_em = silhouette_coeff(new_data_em)
print(not_y_scoeff_em)
print(mean(not_y_scoeff_em))

#K-means
k_out = kmeans(x[,feat_select[1:2]], 4)
labels_k = k_out$cluster
new_data_k = cbind(x[,feat_select[1:2]], labels_k)


plot(new_data_k[,1], new_data_k[,2], col=new_data_k[,3], xlab='x', ylab = 'z')

#apply silhouette coeff
not_y_scoeff_k = silhouette_coeff(new_data_k)
print(not_y_scoeff_k)
print(mean(not_y_scoeff_k))


print(not_y_entr)
print(not_y_hopk)





### without X
feat_select = c(2,3,4)
not_x_entr = entropy_2d(x[,feat_select])
not_x_hopk = hopkins_stat(x[,feat_select])

#EM

gm = mvnormalmixEM(x[,feat_select[1:2]], k=4, epsilon = 1e-4)
plot(gm, which=2)
labels_em=apply(gm$posterior, 1, function(row) which.max(row)) #labels
#apply silhouette coeff
new_data_em = cbind(x[,feat_select[1:2]], labels_em)
not_x_scoeff_em = silhouette_coeff(new_data_em)
print(not_x_scoeff_em)
print(mean(not_x_scoeff_em))

#K-means
k_out = kmeans(x[,feat_select[1:2]], 4)
labels_k = k_out$cluster
new_data_k = cbind(x[,feat_select[1:2]], labels_k)


plot(new_data_k[,1], new_data_k[,2], col=new_data_k[,3], xlab='y', ylab = 'z')

#apply silhouette coeff
not_x_scoeff_k = silhouette_coeff(new_data_k)
print(not_x_scoeff_k)
print(mean(not_x_scoeff_k))


print(not_x_entr)
print(not_x_hopk)
