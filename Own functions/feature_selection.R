rm(list=ls())
set.seed(1337)

root_path = "G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/data-mining-iti8730"

setwd(root_path)


source("./Own functions/silhouette-coeff-fcn.R")
source("./Own functions/entropy-fcn.R")
source("./Own functions/hopkins-stat-fcn.R")
load("./practice_3/Data/3gaussiandata.RData")


library(mixtools)
library(shotGroups) #k-means


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


# library(rgl)
# plot3d(x[,1], x[,2], x[,3], col=x[,4])
# 
# x11()
# plot(x[,1], x[,2])

### all features
feat_select = c(1,2,3,4) #4 is labels
all_feat_scoeff = silhouette_coeff(x[,feat_select])
print(all_feat_scoeff)




### without Z
feat_select = c(1,2,4) 
not_z_scoeff = silhouette_coeff(x[,feat_select])
not_z_entr = entropy_2d(x[,feat_select])
not_z_hopk = hopkins_stat(x[,feat_select])

  #EM
x11()
gm = mvnormalmixEM(x[,feat_select[1:2]], k=2, epsilon = 1e-4)
plot(gm, which=2)
labels_em=apply(gm$posterior, 1, function(row) which.max(row)) #labels
    #apply silhouette coeff
new_data_em = cbind(x[,feat_select[1:2]], labels_em)
not_z_scoeff_em = silhouette_coeff(new_data_em)
print(not_z_scoeff_em)

  #K-means
k_out = kmeans(x[,feat_select[1:2]], 2)
labels_k = k_out$cluster
new_data_k = cbind(x[,feat_select[1:2]], labels_k)

    #apply silhouette coeff
not_z_scoeff_k = silhouette_coeff(new_data_k)
print(not_z_scoeff_k)



print(not_z_scoeff)
print(not_z_entr)
print(not_z_hopk)





### without Y
feat_select = c(1,3,4)
not_y_scoeff = silhouette_coeff(x[,feat_select])
not_y_entr = entropy_2d(x[,feat_select])
not_y_hopk = hopkins_stat(x[,feat_select])

  #EM
x11()
gm = mvnormalmixEM(x[,feat_select[1:2]], k=2, epsilon = 1e-4)
plot(gm, which=2)
labels_em=apply(gm$posterior, 1, function(row) which.max(row)) #labels
    #apply silhouette coeff
new_data_em = cbind(x[,feat_select[1:2]], labels_em)
not_y_scoeff_em = silhouette_coeff(new_data_em)
print(not_y_scoeff_em)

  #K-means
k_out = kmeans(x[,feat_select[1:2]], 2)
labels_k = k_out$cluster
new_data_k = cbind(x[,feat_select[1:2]], labels_k)

    #apply silhouette coeff
not_y_scoeff_k = silhouette_coeff(new_data_k)
print(not_y_scoeff_k)


print(not_y_scoeff)
print(not_y_entr)
print(not_y_hopk)





### without X
feat_select = c(2,3,4)
not_x_scoeff = silhouette_coeff(x[,feat_select])
not_x_entr = entropy_2d(x[,feat_select])
not_x_hopk = hopkins_stat(x[,feat_select])

  #EM
x11()
gm = mvnormalmixEM(x[,feat_select[1:2]], k=2, epsilon = 1e-4)
plot(gm, which=2)
labels_em=apply(gm$posterior, 1, function(row) which.max(row)) #labels
    #apply silhouette coeff
new_data_em = cbind(x[,feat_select[1:2]], labels_em)
not_x_scoeff_em = silhouette_coeff(new_data_em)
print(not_x_scoeff_em)

  #K-means
k_out = kmeans(x[,feat_select[1:2]], 2)
labels_k = k_out$cluster
new_data_k = cbind(x[,feat_select[1:2]], labels_k)

    #apply silhouette coeff
not_x_scoeff_k = silhouette_coeff(new_data_k)
print(not_x_scoeff_k)



print(not_x_entr)
print(not_x_hopk)


# load("./practice_3/Data/JGData.RData")
# print(entropy_2d(x))

# feat_select = c(1,3,4)
# library(factoextra)
# # Compute Hopkins statistic for iris dataset
# res <- get_clust_tendency(x[,feat_select], n = 150, graph = FALSE)
# res$hopkins_stat