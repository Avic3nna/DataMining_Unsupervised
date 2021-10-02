#DBscan

rm(list=ls())
set.seed(1337)




root_path = "G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/data-mining-iti8730"

setwd(root_path)

source("./Own functions/minkowsky-dist.R")
load("./practice_3/Data/JGdata.RData")

no_label_data = x[1:150,1:2]


eps = 1
min_sample=5




get_neighborhood = function(no_label_data, point, eps){
  neighs = list()
  for(i in seq(along=(1:length(no_label_data[,1])))){
    distance = minkowsky(no_label_data[i,],no_label_data[point,],2)
    if(distance <= eps){
      if(i != point){
        neighs = append(neighs, i)
      }
    }
  }
  return(neighs)
}


expand_cluster = function(p, neighbors, label){
  #expand_cluster(p, neighbors,cluster_id)
  
  labels[p] <<- label
  
  for(neighbor in neighbors){
    
    if(labels[neighbor] == -1){
      labels[neighbor] <<- label
    }
    else if(labels[neighbor] == 0){
      labels[neighbor] <<- label
      
      neighbors_of_neighbor = get_neighborhood(no_label_data, neighbor, eps)

      if(length(neighbors_of_neighbor) >= min_sample){
          expand_cluster(neighbor, neighbors_of_neighbor, label)
      }
    }
  }
}


fit_dbscan = function(no_label_data){
  labels <<- array(0, nrow(no_label_data))
  cluster_id = 1
  
  for(p in seq(along=(1:nrow(no_label_data)))){
    if(labels[p] == 0){
      neighbors = get_neighborhood(no_label_data, p, eps)

      if(length(neighbors) < min_sample){
        labels[p] <<- -1
      }
      else{
        expand_cluster(p, neighbors,cluster_id)
        cluster_id = cluster_id + 1
      }
    }
  }
}


fit_dbscan(no_label_data)

x11()
plot(no_label_data[,1], no_label_data[,2], col = factor(labels))


library(fpc)
x11()
fpc::dbscan(no_label_data, MinPts = min_sample, eps, showplot=TRUE)

# neighbors = list()
# neighbors = append(neighbors,1)
# neighbors = append(neighbors,2)
# neighbors = append(neighbors,3)
# for(neighbor in neighbors){
#   print(neighbor)
# }