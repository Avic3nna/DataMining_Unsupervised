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


### Silhouette and intracluster
source("G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/Lab/Own functions/minkowsky-dist.R")

set.seed(1337)
sample_r = train_set[sample(1:nrow(train_set), 50), 1:3]

intra = 0
inter = 0
intra_count = 0
inter_count = 0

for(i in seq(along=(1:(nrow(sample_r)/2)))){
  draw_pair = sample_r[sample(1:nrow(sample_r), 2, replace = FALSE), 1:3]
  #sample without replacement should only select 1 of each
  
  
  Xi = draw_pair[1,]
  Xj = draw_pair[2,]

  Xi_index = which(sample_r[,1:2] == Xi[1:2], arr.ind = TRUE)
  Xj_index = which(sample_r[,1:2] == Xj[1:2], arr.ind = TRUE)

  # print(Xi)
  # print(Xj)
  # 
  # print(Xi_index)
  # print(Xj_index)
  sample_r = sample_r[-c(Xi_index[1], Xj_index[1]),]
  # print(nrow(sample_r))
  # print('')

  if(Xi[3] == Xj[3]){
    intra = intra + minkowsky(Xi, Xj, 2)
    intra_count = intra_count + 1
  }
  else{
    inter = inter + minkowsky(Xi, Xj, 2)
    inter_count = inter_count + 1
  }
}
intra = intra/intra_count
inter = inter/inter_count


intra/inter
