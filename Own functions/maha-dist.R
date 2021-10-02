#DBscan

rm(list=ls())
set.seed(1337)




root_path = "G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/data-mining-iti8730"

setwd(root_path)

source("./Own functions/minkowsky-dist.R")
load("./practice_3/Data/2gaussiandata.RData")


Maha = function(p1, p2, x){
  #Only accepts vectors, 1 observation at a time
  #Don't include labels

  Sigma = cov(x[,(1:(dim(x)[2]))])
  
  if(is.null(dim(p1)) || is.null(dim(p2))){
    Maha = sqrt(t(p1 - p2) %*% (solve(Sigma)) %*% (p1-p2))
  }
  else{
    return("Error: This function doesn't support multiple observations!")
  }
  
  return(Maha)
}




plot(x[,1], x[,2])


p1 = (x[2,1:2])
p2 = (x[40,1:2])

output = Maha(p1,p2,x[,1:2])

print(output)

#1x2 * 2x2 * 2x1


# 
# Euc = minkowsky(p1, p2, 2)
# 
# t(Euc)*solve(Sigma)*Euc
