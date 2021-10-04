# root_path = "G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/data-mining-iti8730"
# 
# setwd(root_path)
# 
# load("./practice_3/Data/2gaussiandata.RData")


maha = function(p1, p2, x){
  #Only accepts vectors, 1 observation at a time
  #Don't include labels

  Sigma = cov(x[,(1:(dim(x)[2]))])
  
  if(is.null(dim(p1)) || is.null(dim(p2))){
    maha = sqrt(t(p1 - p2) %*% (solve(Sigma)) %*% (p1-p2))
  }
  else{
    return("Error: This function doesn't support multiple observations!")
  }
  
  return(maha)
}




# plot(x[,1], x[,2])
# 
# 
# p1 = (x[54,1:2])
# p2 = (x[55,1:2])
# 
# output = Maha(p1,p2,x[,1:2])
# 
# print(output)

