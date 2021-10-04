setwd_current_path = function(){
  library(rstudioapi)
  current_path = getActiveDocumentContext()$path
  setwd(dirname(current_path)) #get this current folder
  setwd('..') #go 1 up for scalability
  print(getwd())
}
setwd_current_path()


source("./Exercise 1. Distance function/oen_minkowski.R")

hopkins_stat = function(X, n=150){
  #takes fcn with labels attached
  no_label_data = X[,-ncol(X)]
  # x11()
  # plot(no_label_data, col = 'black', xlab = 'X1', ylab='X2', ylim=range(c(-5,20)), xlim=range(c(-5,10)))
  
  #find range of dataset for the uniform distrib.
  range_d1 = range(no_label_data[,1])
  range_d2 = range(no_label_data[,2])
  
  
  
  runif_d1 = runif(n, min = range_d1[1], max = range_d1[2])
  runif_d2 = runif(n, min = range_d2[1], max = range_d2[2])
  
  rand_unif = matrix(c(runif_d1, runif_d2), ncol=2)
  #unif_data = data.frame(unif_data)
  # unif_data['label'] = 0
  # 
  # par(new=TRUE)
  # plot(unif_data[,1:2], col = 'green', ylim=range(c(-5,20)), xlim=range(c(-5,10)))
  # 
  # sample_size = n
  
  #https://stackoverflow.com/questions/8273313/sample-random-rows-in-dataframe
  sample_X = no_label_data[sample(nrow(no_label_data), n), ]
  ##rand_unif = unif_data[,-ncol(unif_data)]
  
  #Du distance points q1 ... qm to X
  #Dx distance points p1 ... pm to X
  #H = ( sum(Du) / ( sum(Dx) + sum(Du) ) )

  nn_frame_pi = matrix(NaN, length(no_label_data[,1]), 1) #just 1 column with values
  nn_frame_qi = matrix(NaN, length(no_label_data[,1]), 1) #just 1 column with values
  min_dist_pi_X = matrix(NaN, length(sample_X[,1]))
  min_dist_qi_X = matrix(NaN, length(rand_unif[,1]))
  
  for (i in seq(along = 1:length(sample_X[,1]))){
    for(j in seq(along = 1:length(no_label_data[,1]))){
      
      # distance from sample of X to nearest X
      pi_X_dist = minkowsky(sample_X[i,],no_label_data[j,],2) #eucl. distance
      if(pi_X_dist != 0){                         #don't want same sample dist
        nn_frame_pi[j] = pi_X_dist
      }
      
      # distance from random uniform in range of X to nearest X
      qi_X_dist = minkowsky(rand_unif[i,],no_label_data[j,],2) #eucl. distance
      nn_frame_qi[j] = qi_X_dist
      
    }
    
    min_dist_pi_X[i] = min(na.omit(nn_frame_pi))
    min_dist_qi_X[i] = min(na.omit(nn_frame_qi))
  }
  
  H = sum(min_dist_qi_X)/(sum(min_dist_pi_X) + sum(min_dist_qi_X))
  return(H)
  
}