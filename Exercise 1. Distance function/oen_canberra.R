# canberra function
canberra = function(p,q){
  d = sum((abs(p - q)/(abs(p) + abs(q))), na.rm=T)
  return(d)
}


# p = c(1, 2, 3)
# q = c(4, 5, 6)
# 
# canberra_dist = canberra(p,q)
# canberra_dist
