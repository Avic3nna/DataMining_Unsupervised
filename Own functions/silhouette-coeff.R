# this file is to implement my own clustering 
rm(list=ls())
graphics.off()
source("G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/Lab/Own functions/minkowsky-dist.R")
set.seed(1337)

# we can keep here our distance function
# get_distance <-function(element1,element2,metricf){
#   # this function returns the distace between the element1 and element2
#   # according to the metricf
#   dimensions=length(element1)
#   sqd<-matrix(, dimensions,1)
#   if (metricf=="Euclidean"){
#     for(i in seq(along=element1)){
#       sqd[i]<-(element1[i]-element2[i])^2
#     }
#     dist<-sqrt(colSums(sqd))
#   }
#   # please add here Manhattan, Canberra, Minkowsky and whatever distances are appropriate
#   return(dist)
# }


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
# loading the data from file
load(file="G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/Lab/Own functions/data/kdata.RData")


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
# Assume that K = 3
K <- 3 
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

while(mean(abs(centroids-old_centroids)) >= 1e-4){
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



x11()
plot(train_set[, 1:2], type='p') #col = train_set[,3]
par(new=TRUE)
points(centroids[,1], centroids[,2], col="red", type='p')


### Silhouette coefficient

# cluster1 = train_set[(train_set[,3] == 1), 1:2]
# cluster2 = train_set[(train_set[,3] == 2), 1:2]
# cluster3 = train_set[(train_set[,3] == 3), 1:2]
# 
# cluster_list = list(cluster1, cluster2, cluster3)
# #D_avg_in = average distance of Xi
# 
# 
# avg_dist_per_cluster = array(NA, dim = length(unique(train_set[,3])))
# list_nr = 1
# for(cluster in cluster_list){
#   avg_dist = matrix(NA, nrow(cluster), 1)
#   for(i in seq(along=(1:nrow(cluster)))){
#     for(j in seq(along=(1:nrow(cluster))))
#     {
#       if(i != j){
#         avg_dist[i] = minkowsky(cluster[i,], cluster[j,], 2)
#       }
#     }
#   }
#   avg_dist_per_cluster[list_nr] = sum(avg_dist)/nrow(cluster)
#   list_nr = list_nr + 1
# }
# avg_dist_per_cluster
# 
# # D_min_out minimum of the average distances over other clusters
# 
# avg_dist = matrix(NA, nrow(train_set), 1)
# avg_min_dist = array(NA, dim = length((train_set[,3])))
# list_nr = 1
# for(i in seq(along=(1:nrow(train_set)))){
#   for(j in seq(along=(1:nrow(train_set))))
#   {
#     if(train_set[i,3] != train_set[j,3]){ # not in the same cluster
#       avg_dist[i] = minkowsky(train_set[i, 1:2], train_set[j,1:2], 2)
#     }
#   }
# }
# avg_min_dist[list_nr] = sum(avg_dist)/nrow(train_set)
# list_nr = list_nr + 1
# 
# avg_total_dist

# Combine the code? Do it per datapoint

classes = unique(train_set[,3])
classes_amount = length(classes)
S = array(NA, nrow(train_set))

amount_in = 0
amount_out = 0
for(i in seq(along=(1:nrow(train_set)))){
  avg_dist_out = matrix(NA, nrow(train_set), 2) #dist and cluster #
  avg_dist_in = matrix(NA, nrow(train_set), 1)
  for(j in seq(along=(1:nrow(train_set))))
  {
    if(train_set[i,3] != train_set[j,3]){ # not in the same cluster
      avg_dist_out[j,1] = minkowsky(train_set[i, 1:2], train_set[j, 1:2], 2)
      avg_dist_out[j,2] = train_set[j,3]
      amount_out = amount_out + 1
    }
    else{
      avg_dist_in[j] = minkowsky(train_set[i, 1:2], train_set[j,1:2], 2)
      amount_in = amount_in + 1
    }

  }
  
  avg_dist_in = na.omit(avg_dist_in)
  avg_dist_out = na.omit(avg_dist_out)
  
  #split avg dist out in the remaining classes and get the avg distance per class
  #and choose the minimum one for calc
  
  class_type = unique(avg_dist_out[,2])
  
  # remaining_class_range = 1:(classes_amount-1)
  type1 = avg_dist_out[(avg_dist_out[,2] == class_type[1]),1]
  type2 = avg_dist_out[(avg_dist_out[,2] == class_type[2]),1]
  
  avg_cluster_type1 = sum(type1)/length(type1)
  avg_cluster_type2 = sum(type2)/length(type2)
  
  D_min_out = min(c(avg_cluster_type1, avg_cluster_type2))
  D_avg_in = sum(avg_dist_in)/amount_in
    
  S[i] = (D_min_out-D_avg_in)/(max(c(D_avg_in, D_min_out)))
}

Silhouette = sum(S)/length(S)


### RETURNS 1 FOR NICELY SEPERATED DATA, CHECK IF THIS GETS WORSE WITH OVERLAP