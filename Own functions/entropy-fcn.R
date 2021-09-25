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
  if (sum(p) != 1){
    return(NaN)
  }
  E = -sum(p*log(p), na.rm=TRUE)
  E = E/log(length(p)) #scale entropy by log(# of classes)
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
total_prob = c(0, 0, 0, 1)


print(entropy(total_prob))


