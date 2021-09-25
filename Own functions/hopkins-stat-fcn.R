source("G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/Lab/Own functions/minkowsky-dist.R")

hopkins_stat = function(X, sample_X, rand_unif){
  #Du distance points q1 ... qm to X
  #Dx distance points p1 ... pm to X
  #H = ( sum(Du) / ( sum(Dx) + sum(Du) ) )
  if(length(sample_X[,1]) != length(rand_unif[,1]))
    return(NaN)
  
  nn_frame_pi = matrix(NaN, length(X[,1]), 1) #just 1 column with values
  nn_frame_qi = matrix(NaN, length(X[,1]), 1) #just 1 column with values
  min_dist_pi_X = matrix(NaN, length(sample_X[,1]))
  min_dist_qi_X = matrix(NaN, length(rand_unif[,1]))
  
  for (i in seq(along = 1:length(sample_X[,1]))){
    for(j in seq(along = 1:length(X[,1]))){
      
      # distance from sample of X to nearest X
      pi_X_dist = minkowsky(sample_X[i,],X[j,],2) #eucl. distance
      if(pi_X_dist != 0){                         #don't want same sample dist
        nn_frame_pi[j] = pi_X_dist
      }
      
      # distance from random uniform in range of X to nearest X
      qi_X_dist = minkowsky(rand_unif[i,],X[j,],2) #eucl. distance
      nn_frame_qi[j] = qi_X_dist
      
    }
    
    min_dist_pi_X[i] = min(na.omit(nn_frame_pi))
    min_dist_qi_X[i] = min(na.omit(nn_frame_qi))
  }
  
  H = sum(min_dist_qi_X)/(sum(min_dist_pi_X) + sum(min_dist_qi_X))
  return(H)
  
}