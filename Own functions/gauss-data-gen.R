# Omar El Nahhas
# 18/09/2021
# Data Mining ITI8730
# Generate 2D dataset from Gaussian distribution

rm(list = ls())

library(mvtnorm) #multivariate normal distribution library
set.seed(1337)

multi_gauss_data_gen = function(n, mu, covar){
  if(length(mu) != length(covar)){ #requires equal dim. 
    return(NaN)
  }
  
  #matrix frame that will contain all the data
  output = matrix(data=NA,n*length(mu),length(mu[[1]])+1)
  
  for(j in seq(along = 1:length(mu))){
    #returns n*length(mu[[j]]) coords
    mvn = rmvnorm(n=n, mean=mu[[j]], sigma=covar[[j]])
    
    #insert the generated data inside the x matrix and add label
    for (i in seq(along=1:n)){
      output[(j-1)*n+i,1:length(mu[[j]])] = mvn[i,]
      output[(j-1)*n+i,length(mu[[j]])+1] = j
    }
  }
  return(output)
}


### CONFIGURATION OF GAUSSIAN
n = 200 #samples per cluster

mu_2D = list(c(2,0),  #mu1
             c(1,10), #mu2
             c(10,7), #mu3
             c(11,0), #mu4
             )
                      #...
                      

#needs to be positive semi-definite
covar = list(matrix(c(4,2,2,3),ncol=length(mu_2D[[1]])),    #sigma1
              matrix(c(2,-2,-2,6),ncol=length(mu_2D[[1]])), #sigma2
              matrix(c(4,-4,-4,5),ncol=length(mu_2D[[1]])), #sigma3
             matrix(c(10,4,4,3),ncol=length(mu_2D[[1]]))),  #sigma4
                                                            #...
                                                            
### END CONFIGURATION OF GAUSSIAN

#generation of gaussians, returns NaN if input dimensions don't match
generated_data = multi_gauss_data_gen(n, mu_2D, covar)

#plot clusters
x11() #remove x11() if using MacOS
plot(generated_data, col=generated_data[,ncol(generated_data)])

#remove labels
no_label_data = generated_data[,-ncol(generated_data)]
