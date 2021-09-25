# clustering and different distance functions. 
# clear everything
rm(list=ls())

library(shotGroups)
# load the data
load(file="/Users/sven/Google Drive/Teaching/Data_Mining_2021/Practice_02/kdata.RData")
# NB!!!!!!!!   variable x will appear in the workspace.
#split the data in proportion 70/30 for training and validation purposes.
sample_size <- floor(0.7 * nrow(x))

#set.seed(123) # seed is necessary to make it reproducible
train_ind <- sample(seq_len(nrow(x)), size = sample_size)
train_ind

train_set <- x[train_ind, ]
test <- x[-train_ind, ]
train <- train_set[,1:2] 

#results <- kmeans(train,3) #k-means needs to know k value, number of clusters
#idx = results[["cluster"]]

#for (i in seq(along=idx)){
#  a<-switch(idx[i],"red","green","blue") 
#  plot(train[i,1],train[i,2], col=a,type="p",xlim=c(-10,20),ylim=c(-10,20))
#  par(new=TRUE)
#  }

# select cluster Nr 1 (you may select any other cluster)
#cluster_1 = train[idx == 1,]

#cov_cluster_1 = cov(cluster_1)

#drawEllipse(results$centers[1,], cov_cluster_1, radius=3, nv = 100, axes = FALSE, fg = par('fg'), bg = NA, colCtr = "red", lty = par('lty'), lwd = par('lwd'), pch = par('pch'), cex = par('cex'))
# K-Means Clustering with 5 clusters
fit <- kmeans(train, 3)


library(cluster)
clusplot(train, fit$cluster, color=TRUE, shade=TRUE,
        labels=3, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
#plotcluster(train, fit$cluster)
