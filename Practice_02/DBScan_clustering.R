# clear everything
rm(list=ls())

library(shotGroups)
# load the data
load(file="/Users/sven/Google Drive/Teaching/Data_Mining_2021/Practice_02/kdata.RData") #var x


plot(x[,1],x[,2], ylim=range(c(-5,65)),xlim=range(c(-10,15)))


set.seed(123) # seed is necessary to make it reproducible

#data
train <- x # the data we used was initially prepared for the classification example please remove third column

#k-means

results <- kmeans(train,3)
idx = results[["cluster"]]

for (i in seq(along=idx)){
  a<-switch(idx[i],"red","green", "blue") 
  plot(train[i,1],train[i,2], col=a,type="p",ylim=range(c(0,65)),xlim=range(c(-10,15)))
  par(new=TRUE)
}

#install fpc package
#dbscan

#results <- fpc::dbscan(x[,1:2], eps = 3, MinPts = 6)
#Plot DBSCAN results
#plot(results, train, main = "DBSCAN CLUSTERING", frame = FALSE, ylim=range(c(0,65)),xlim=range(c(-10,15)))

results <- fpc::dbscan(x[,1:2], eps = 3, MinPts = 12)
#plot DBSCAN results
plot(results, train, main = "DBSCAN CLUSTERING", frame = FALSE, ylim=range(c(0,65)),xlim=range(c(-10,15)))

#results <- fpc::dbscan(x[,1:2], eps = 2, MinPts = 10)
#Plot DBSCAN results
#plot(results, train, main = "DBSCAN CLUSTERING", frame = FALSE, ylim=range(c(0,65)),xlim=range(c(-10,15)))
