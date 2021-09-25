library(mvtnorm)

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