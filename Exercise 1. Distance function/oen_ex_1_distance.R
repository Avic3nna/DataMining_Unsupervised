rm(list=ls())
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

source("./Exercise 1. Distance function/oen_canberra.R")
source("./Exercise 1. Distance function/oen_mahalanobis.R")
source("./Exercise 1. Distance function/oen_minkowski.R")

load("./Data/3Dgauss.RData")

x = generated_data #above data is called generated_data, change accordingly

### Points for minkowsky and canberra
p = c(1, 2, 3)
q = c(4, 5, 6)

print(canberra(p,q)) #Canberra
print(minkowsky(p,q,1)) #Minkowski -> Manhattan
print(minkowsky(p,q,2)) #Minkowski -> Euclidian
print(minkowsky(p,q,500)) #Minkowski -> Chebyshev

### Points for mahalanobis
p_m = x[1,1:3] #no label
q_m = x[1337, 1:3] #no label

print(maha(p_m, q_m, x[,1:3])) #don't send label with
