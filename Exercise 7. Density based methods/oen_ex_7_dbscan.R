# Source 1: https://github.com/eriklindernoren/ML-From-Scratch/blob/master/mlfromscratch/unsupervised_learning/dbscan.py
# Source 2: https://towardsdatascience.com/understanding-dbscan-algorithm-and-implementation-from-scratch-c256289479c5
# Made together with Javier Galindos - professor has been notified about this

rm(list=ls())
set.seed(1337)

packages_used = c("rstudioapi",
                  "rgl")

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

library(rgl)

source("./Exercise 1. Distance function/oen_minkowski.R")

load("./Data/3Dgauss.RData")

x = generated_data
dataset = x


eps = 2
min_samples = 10

# Get neighbors
get_neighbors = function(dataset, sample_i, eps){
  #     Return a list of indexes of neighboring samples
  #     A sample_2 is considered a neighbor of sample_1 if the distance between
  #     them is smaller than epsilon "
  neighbors_append = list()
  for (i in seq(along=1:nrow(dataset))){
    distance = minkowsky(dataset[i,], dataset[sample_i,],2)
    if (distance <= eps){
       # Append neighbor
      neighbors_append = c(neighbors_append, i)
    }
  }
  return (neighbors_append)
}


expand_cluster = function(dataset, sample_i){
  # Recursive method which expands the cluster until we have reached the border
  # of the dense area (density determined by eps and min_samples) 
  cluster = list(sample_i)
  # Iterate through neighbors
  for (neighbor_i in neighbors[[sample_i]]){
      if (! (neighbor_i %in% visited_samples)){
        visited_samples <<-c(visited_samples, neighbor_i) # Append
        # Fetch the sample's distant neighbors (neighbors of neighbor)
        neighbors[[neighbor_i]] <<- get_neighbors(dataset, neighbor_i, eps)
        # Make sure the neighbor's neighbors are more than min_samples
        # (If this is true the neighbor is a core point)
        if (length(neighbors[[neighbor_i]]) >= min_samples){
          # Expand the cluster from the neighbor
          expanded_cluster = expand_cluster(dataset,neighbor_i)
          # Add expanded cluster to this cluster
          cluster = c(cluster, expanded_cluster)
        }
        else{
          # If the neighbor is not a core point we only add the neighbor point
          cluster = c(cluster,neighbor_i)
        }
      }
  }
  return (cluster)
}


get_cluster_labels =function(clusters){
  # Return the samples labels as the index of the cluster in which they are contained
  # Set default value to number of clusters
  # Will make sure all outliers have same cluster label
  labels = array((length(clusters)+1),dim=nrow(dataset))
  for (cluster_i in seq_along(clusters)){
    for (sample_i in clusters[[cluster_i]]){
      labels[sample_i] = cluster_i
    }
  }
  labels[is.na(labels)]=length(clusters)+1
  return (labels)
}


# DBSCAN
predictDBSCAN = function(dataset,eps,min_samples){
  # <<- to make global variables
  clusters <<- list()
  visited_samples <<- list()
  neighbors <<- list()
  n_samples = nrow(dataset)
  # Iterate through samples and expand clusters from them
  # if they have more neighbors than self.min_samples
  for (sample_i in seq(1:n_samples)){
    if (sample_i %in% visited_samples){
      next
    }
      
    neighbors[[sample_i]] <<- get_neighbors(dataset,sample_i,eps)
    if (length(neighbors[[sample_i]]) >= min_samples){
      # If core point => mark as visited
      visited_samples <<-c(visited_samples, sample_i) # Append
      # Sample has more neighbors than self.min_samples => expand
      # cluster from sample
      new_cluster = list(expand_cluster(dataset, sample_i))
      # Add cluster to list of clusters
      clusters <<- c(clusters,new_cluster)
    }
  }
  
  # Get the resulting cluster labels
  cluster_labels = get_cluster_labels(clusters)
  return (cluster_labels)
}

out=predictDBSCAN(x,eps,min_samples)

# black = 1
# red = 2
# green = 3


new = out
new = new + 1
new[new == (length(unique(new)))] = 1
plot3d(x[,1],x[,2],x[,3],col= factor(new), xlab='x', ylab='y', zlab='z')

