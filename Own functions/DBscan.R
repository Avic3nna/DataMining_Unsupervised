#DBscan

rm(list=ls())
set.seed(1337)




root_path = "G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/data-mining-iti8730"

setwd(root_path)

source("./Own functions/minkowsky-dist.R")
load("./practice_3/Data/JGdata.RData")

no_label_data = x[1:50,1:2]


eps = 1
min_sample=5


# Get neighbors
update_labels = function(no_label_data, pointId, eps, labels, cluster_val){
  #     Return a list of indexes of neighboring samples
  #     A sample_2 is considered a neighbor of sample_1 if the distance between
  #     them is smaller than epsilon "
  direct_neighbours = list()
  label_index = list()
  for (i in seq(along=1:nrow(no_label_data))){
    distance = minkowsky(no_label_data[i,],no_label_data[pointId,],2)
    if (distance < eps){
      # Append neighbor
      direct_neighbours = c(direct_neighbours, no_label_data[i])
      label_index = c(label_index, i)
    }
  }
  if(length(direct_neighbours) < min_sample){
    for(i in seq(along=(1:length(labels)))){
      if(i %in% label_index){
        labels[i] = -1
      }
    }
  }
  else{
    for(i in seq(along=(1:length(labels)))){
      if(i %in% label_index){
        labels[i] = cluster_val
        #update_labels(no_label_data, i, eps, labels, cluster_val)
      }
    }
  }
  return (labels)
}



labels = array(0, nrow(no_label_data))
C = 1

for(p in seq(along=(1:nrow(no_label_data)))){
  if(labels[p] == 0){
    labels = update_labels(no_label_data, p, eps, labels, C)
    C = C+1
  }
}

x11()
plot(no_label_data[,1], no_label_data[,2], col = factor(labels))

library(fpc)
x11()
fpc::dbscan(no_label_data, MinPts = min_sample, eps, showplot=TRUE)
