packages_used = c("rstudioapi")

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


silhouette_coeff = function(train_set){
  classes = unique(train_set[,ncol(train_set)])
  classes_amount = length(classes)
  S = array(NA, nrow(train_set))
  
  
  for(i in seq(along=(1:nrow(train_set)))){
    amount_in = 0
    amount_out = 0
    avg_dist_out = matrix(NA, nrow(train_set), 2) #dist and cluster #
    avg_dist_in = matrix(NA, nrow(train_set), 1)
    for(j in seq(along=(1:nrow(train_set))))
    {
      if(train_set[i,ncol(train_set)] != train_set[j,ncol(train_set)]){ # not in the same cluster
        avg_dist_out[j,1] = minkowsky(train_set[i, 1:(ncol(train_set)-1)], train_set[j, 1:(ncol(train_set)-1)], 2)
        avg_dist_out[j,2] = train_set[j,ncol(train_set)]
        amount_out = amount_out + 1
      }
      else{
        avg_dist_in[j] = minkowsky(train_set[i, 1:(ncol(train_set)-1)], train_set[j,1:(ncol(train_set)-1)], 2)
        amount_in = amount_in + 1
      }
      
    }
    
    avg_dist_in = na.omit(avg_dist_in)
    avg_dist_out = na.omit(avg_dist_out)
    
    #split avg dist out in the remaining classes and get the avg distance per class
    #and choose the minimum one for calc
    
    class_type = unique(avg_dist_out[,2])
    
    # remaining_class_range = 1:(classes_amount-1)
    avg_cluster_type = array(NA, dim = length(class_type))
    
    for(y in seq(along=1:length(class_type))){
      type = avg_dist_out[(avg_dist_out[,2] == class_type[y]),1]
      avg_cluster_type[y] = sum(type)/length(type)
    }
    
    
    D_min_out = min(c(avg_cluster_type))
    D_avg_in = sum(avg_dist_in)/amount_in
    
    S[i] = (D_min_out-D_avg_in)/(max(c(D_avg_in, D_min_out)))
  }
  
  #Silhouette = sum(S)/length(S)
  
  #print(Silhouette)
  
  final_data = cbind(train_set, S)
  
  lengte = length(unique(train_set[,ncol(train_set)]))
  cluster_s_coeff = array(NA, dim = lengte)
  
  for(d in seq(along = 1:lengte)){
    cluster_s_coeff[d] = mean(final_data[final_data[,ncol(final_data)-1] == d, ncol(final_data)])
  }
  return(cluster_s_coeff)
}

# print(silhouette_coeff(train_set))
