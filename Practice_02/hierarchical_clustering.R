# clear everything
rm(list=ls())

library(shotGroups)
# load the data
load(file="/Users/sven/Google Drive/Teaching/Data_Mining_2021/Practice_02/kdata.RData") #var x


plot(x[,1],x[,2], ylim=range(c(-5,65)),xlim=range(c(-10,15)))

sample_size <- floor(0.8 * nrow(x))

sampled_values <- sample.int(nrow(x), sample_size, replace=FALSE)
x <- x[sampled_values, ]

plot(x[,1],x[,2], ylim=range(c(-5,65)),xlim=range(c(-10,15)))

set.seed(123) # seed is necessary to make it reproducible
clusters <- hclust(dist(x[,1:2]))
plot(clusters)

clusterCut <- cutree(clusters, 3)
table(clusterCut, x[,1])

clusters <- hclust(dist(x[,1:2]), method = 'average')
plot(clusters)

print(nrow(x))

x<-x[-515,]

print(nrow(x))

clusters <- hclust(dist(x[,1:2]), method = 'average')
plot(clusters)

print(nrow(x))

x<-x[-300,]

print(nrow(x))

clusters <- hclust(dist(x[,1:2]))
plot(clusters)