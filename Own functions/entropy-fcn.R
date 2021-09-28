#Entropy = prob_s1 * surprise_s1 + prob_s2*surprise_s2


# p_i is the proportion of the points in the region i, m - total
# number of regions. Large values of E indicate poor clustering
# behaviour.
# for every cluster has a proportion of points with a certain label (p_i), 
# compute:
rm(list = ls())

# entropy_binary = function(p){
#   if (min(p) < 0 || sum(p) <= 0 || sum(p) > 1){
#     return(NaN)
#   }
#   #only holds true in binary classification
#   E = -sum(p*log(p) + (1-p)*log(1-p), na.rm=TRUE)
#   return(E)
# }

entropy = function(p){
  #requires sum of prob. to be equal to 1
  
  # freq = table(no_label_data)/length(no_label_data)
  # 
  # vec = as.data.frame()
  # 
  
  if (sum(p) != 1){
    return(NaN)
  }
  E = -sum(p*log(p), na.rm=TRUE)
  E = E/log2(length(p)) #scale entropy by log(# of classes)
  return(E)
}

# The maximum value of entropy is logk, 
# where k is the number of categories you are using. 
# Its numeric value will naturally depend on the base of logarithms 
# you are using.
# 
# Using base 2 logarithms as an example, as in the question: 
#   log2(1) is 0 and log2(2) is 1, 
# so a result greater than 1 is definitely wrong 
# if the number of categories is 1 or 2. 
# A value greater than 1 will be wrong if it exceeds log2(k).
# 
# In view of this it is fairly common to scale entropy by log(k), 
# so that results then do fall between 0 and 1,

# f.e. 4 classes with their probabilities of occuring in this cluster


################### ENTROPY


set.seed(1337)

root_path = "G:/My Drive/1. EIT Digital master/Estland/Semester 1/Data mining/data-mining-iti8730"

setwd(root_path)

#source("./Own functions/minkowsky-dist.R")
load("./practice_3/Data/JGdata.RData")

no_label_data = x[,1:2]

k_dims = 2

phi_regions = 10

m_grid_ranges = 1:phi_regions**k_dims

#p_i = faction of data points in grid region i

range_d1 = range(no_label_data[,1]) #x
range_d2 = range(no_label_data[,2]) #y

start_x = min(range_d1)
end_x = max(range_d1)

start_y = min(range_d2)
end_y = max(range_d2)

total_x = abs(end_x - start_x)
total_y = abs(end_y - start_y)

grid_cell_x = total_x/phi_regions
grid_cell_y = total_y/phi_regions

min_x = start_x
min_y = start_y

points_in_cell = array(NA, phi_regions**k_dims)
for(i in m_grid_ranges){ 
  #100 grids, every 10 you start at 0
  
  if(i %in% seq(11,91, by=10)){
    min_y = min_y + grid_cell_y
    min_x = start_x
  }

  max_x = min_x + grid_cell_x
  max_y = min_y + grid_cell_y

  

  #loop through all data points, check if they are in grid
  counter = 0
  for(j in seq(along=1:nrow(no_label_data))){
    x = no_label_data[j,1]
    y = no_label_data[j,2]
    
    if(x < max_x && x >= min_x){
      if(y < max_y && y >= min_y){
        counter = counter + 1
      }
    }
  }
  min_x = min_x + grid_cell_x
  points_in_cell[i] = counter
}

probability = points_in_cell/nrow(no_label_data)
print(entropy(probability))