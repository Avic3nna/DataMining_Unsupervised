
# clear everything
rm(list=ls())

library(shotGroups)
# load the data
load(file="G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/Lab/Practice_02/Javier.RData") #var x

x11()
plot(x[,1],x[,2])

set.seed(1337) # seed is necessary to make it reproducible

#data
train <- x # the data we used was initially prepared for the classification example please remove third column

#k-means

results <- kmeans(train,8)
idx = results[["cluster"]]

x11()
plot(train[,1], train[,2], col = idx, type='p')


#dataset with labels using k-means
train <- cbind(train,class=idx)



library(cluster)
silhouette(train[,3])

# for (i in seq(along=idx)){
#   plot(train[i,1],train[i,2], col=idx[i],type="p")
#   par(new=TRUE)
# }

#install fpc package
#dbscan
library(fpc)
results <- fpc::dbscan(x[,1:2], eps = 3, MinPts = 6)
#Plot DBSCAN results
x11()
plot(results, train, main = "DBSCAN CLUSTERING", frame = FALSE)

results <- fpc::dbscan(x[,1:2], eps = 3, MinPts = 12)
#plot DBSCAN results
x11()
plot(results, train, main = "DBSCAN CLUSTERING", frame = FALSE)

#results <- fpc::dbscan(x[,1:2], eps = 2, MinPts = 10)
#Plot DBSCAN results
#plot(results, train, main = "DBSCAN CLUSTERING", frame = FALSE, ylim=range(c(0,65)),xlim=range(c(-10,15)))


library(clValid)

intern = clValid(train[, 1:2], 4:8, clMethods = c("kmeans", "hierarchical", "pam"), validation="internal")

summary(intern)
# pam = partition around medoids