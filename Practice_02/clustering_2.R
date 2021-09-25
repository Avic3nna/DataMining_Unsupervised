rm(list=ls())

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

# Ward Hierarchical Clustering
d <- dist(train, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")

plot(train, pch=20, col=cutree(fit,3))






#plot(fit) # display dendogram
#groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
#library(cluster)
#clusplot(train, fit$cluster, color=TRUE, shade=TRUE,
#         labels=3, lines=0)
#rect.hclust(fit, k=5, border="red")

#ds <- dbscan(train, 0.7)
#plot(train[ds$cluster %in% 1:3,])
#plot(train, col=ds$cluster)