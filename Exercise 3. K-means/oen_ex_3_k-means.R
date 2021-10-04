rm(list=ls())
set.seed(1337)

packages_used = c("rstudioapi") #

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


source("./Exercise 1. Distance function/oen_minkowski.R")

load("./Data/3Dgauss.RData")

x = generated_data[,1:2]



# function to find the label
get_label <- function(dataset, centroids){
  for (i in seq(along=1:nrow(dataset))){
    temp_dist <- matrix(NaN,nrow=nrow(centroids))
    for (j in seq(along=1:nrow(centroids))){
      temp_dist[j,] = minkowsky(centroids[j,], dataset[i,1:2], 2)
    }
    dataset[i, 3] = which.min(temp_dist)
  }
  return(dataset)
}

# update centroids on each iteration
update_centroids <- function(dataset){
  last_column = ncol(dataset)
  K = max(dataset[,last_column])
  centroids <- matrix(NaN, nrow=K, ncol=last_column-1)
  for (k in seq(along=1:K)){
    cluster <- dataset[dataset[, last_column] == k, 1:2]
    
    centroids[k,] = colMeans(cluster)
    
  }
  # cat("___")
  # cat(centroids)
  # cat("___")
  return(centroids)
}

# do we need the following step?
#split the data in proportion 70/30 for training and validation purposes.
sample_size <- floor(0.7 * nrow(x))
data_dimensionality = ncol(x)
#set.seed(123) # seed is necessary to make it reproducible
train_ind <- sample(seq_len(nrow(x)), size = sample_size)

train_set <- x[train_ind, ]
test <- x[-train_ind, ]
train <- train_set[,1:2] # just to assure that we have two columns

# initialize
# Assume that K = 4
K <- 4 
centroids <- matrix(NaN, nrow=K,ncol=data_dimensionality)


for (k in seq(along=1:K)){
  centroids[k,] = runif(2)
}
cat("Initial centroids are: ", centroids)

# this example does not contain any convergence criteria
# students are asked to design and add it.

# add one column (to be used for labels)
aux_column <- matrix(0, nrow(train_set))
train_set = cbind(train_set,aux_column)

i = 0
old_centroids = 0

while(mean(abs(centroids-old_centroids)) >= 1e-8){
  cat("Step:", i)
  old_centroids = centroids
  train_set <- get_label(train_set, centroids)
  centroids <- update_centroids(train_set)
  cat(" Centroids have been updated:", centroids)
  cat(" Change: ", mean(abs(centroids-old_centroids)))
  cat('\n')
  #plot(centroids, col="red")
  #par(new=TRUE)
  i = i+1
}




plot(train_set[, 1:2], type='p') #col = train_set[,3]
par(new=TRUE)
points(centroids[,1], centroids[,2], col="red", type='p')

