#Hopkins statistics

rm(list = ls())
library(mvtnorm) #multivariate normal distribution library
set.seed(1337)

source("G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/Lab/Own functions/minkowsky-dist.R")
root_path = "G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/data-mining-iti8730"

setwd(root_path)

source("./Own functions/minkowsky-dist.R")
load("./practice_3/Data/JGdata.RData")
### CONFIGURATION OF GAUSSIAN

no_label_data = x[,2]

no_label_data = matrix(runif(200,1,100),1500,1) #uniform data
n = 400 #samples per cluster



#find range of dataset for the uniform distrib.

### NOTE IF MORE DIMENSIONS YOU NEED TO ADD d3 etc etc
range_d1 = range(no_label_data[,1])
range_d2 = range(no_label_data[,2])



runif_d1 = runif(n/4, min = range_d1[1], max = range_d1[2])
runif_d2 = runif(n/4, min = range_d2[1], max = range_d2[2])

unif_data = matrix(c(runif_d1), ncol=1)
unif_data = data.frame(unif_data)
unif_data['label'] = 0

# par(new=TRUE)
# plot(unif_data[,1:2], col = 'green', ylim=range(c(-5,20)), xlim=range(c(-5,10)))

sample_size = n/4

#https://stackoverflow.com/questions/8273313/sample-random-rows-in-dataframe
sample_X = no_label_data[sample(nrow(no_label_data), sample_size)]
rand_unif = unif_data[,-ncol(unif_data)]

# par(new=TRUE)
# plot(sample_X[,1:2], col = 'red', xlab = 'X1', ylab='X2', ylim=range(c(-5,20)), xlim=range(c(-5,10)))

# 1. Select random m data points from X (p1 .. pm) (gaussian clusters)
# 2. Generate randomly m data points (q1 .. qm) for uniform distribution within same range as X
# 3. Calculate distances (p1 .. pm to points in X, q1 .. qm to points in X)
# 4. Use params in hopkins statistic

hopkins_stat = function(X, sample_X, rand_unif){
  #Du distance points q1 ... qm to X
  #Dx distance points p1 ... pm to X
  if(length(sample_X) != length(rand_unif))
    return(NaN)
  
  nn_frame_pi = matrix(NaN, length(X), 1) #just 1 column with values
  nn_frame_qi = matrix(NaN, length(X), 1) #just 1 column with values
  min_dist_pi_X = matrix(NaN, length(sample_X))
  min_dist_qi_X = matrix(NaN, length(rand_unif))
  
  for (i in seq(along = 1:length(sample_X))){
    for(j in seq(along = 1:length(X))){
      
      # distance from sample of X to nearest X
      pi_X_dist = minkowsky(sample_X[i],X[j],2) #eucl. distance
      if(pi_X_dist != 0){                         #don't want same sample dist
        nn_frame_pi[j] = pi_X_dist
      }
      
      # distance from random uniform in range of X to nearest X
      qi_X_dist = minkowsky(rand_unif[i],X[j],2) #eucl. distance
      nn_frame_qi[j] = qi_X_dist
    }
    
    min_dist_pi_X[i] = min(na.omit(nn_frame_pi))
    min_dist_qi_X[i] = min(na.omit(nn_frame_qi))
  }
  
  H = sum(min_dist_qi_X)/((sum(min_dist_qi_X)) + sum(min_dist_pi_X))
  return(H)
  
}

print(hopkins_stat(no_label_data, sample_X, rand_unif))

# H = 1: data highly clustered
# H = 0.5: data uniform
# H = 0: data random

#check with native formula
# library(clustertend)
# hopkins(no_label_data, 20, header=TRUE)

library(factoextra)
# Compute Hopkins statistic for iris dataset
res <- get_clust_tendency(no_label_data, n = n/4, graph = FALSE)
res$hopkins_stat

# 
# plot(no_label_data[,1], no_label_data[,2])
# par(new=TRUE)
# plot(rand_unif[,1], rand_unif[,2])
