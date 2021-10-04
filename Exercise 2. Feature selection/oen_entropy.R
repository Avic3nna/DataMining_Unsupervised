entropy_calc = function(p){
  #requires sum of prob. to be equal to 1
  if (sum(p) != 1){
    return(NaN)
  }
  E = -sum((p*log(p))+ (1-p)*log(1-p), na.rm=TRUE)
  #E = E/log(length(p)) #scale entropy by log(# of classes)
  return(E)
}


entropy_2d = function(x, phi_regions=10,k_dims=2)
{
  no_label_data = x[,1:2]
  
  # plot(no_label_data[,1], no_label_data[,2])
  
  # k_dims = 2
  # 
  # phi_regions = 10
  
  m_grid_ranges = 1:phi_regions**k_dims
  
  #p_i = faction of data points in grid region i
  
  range_d1 = range(no_label_data[,1]) #x
  range_d2 = range(no_label_data[,2]) #y
  
  cell_margin = 0.1
  
  start_x = min(range_d1) - cell_margin
  end_x = max(range_d1) + cell_margin
  
  start_y = min(range_d2) - cell_margin
  end_y = max(range_d2) + cell_margin
  
  total_x = abs(end_x - start_x)
  total_y = abs(end_y - start_y)
  
  grid_cell_x = total_x/phi_regions
  grid_cell_y = total_y/phi_regions
  
  min_x = start_x
  min_y = start_y
  
  points_in_cell = array(NA, phi_regions**k_dims)
  for(i in m_grid_ranges){ 
    #100 grids, every 10 you start at 0
    
    if(i %in% seq(phi_regions+1,((phi_regions**k_dims)+1)-phi_regions, by=phi_regions)){
      min_y = min_y + grid_cell_y
      min_x = start_x
    }
    
    max_x = min_x + grid_cell_x
    max_y = min_y + grid_cell_y
    
    
    # print(c(min_x,max_x))
    # print(c(min_y, max_y))
    # print('')
    #loop through all data points, check if they are in grid
    counter = 0
    for(j in seq(along=1:nrow(no_label_data))){
      x = no_label_data[j,1]
      y = no_label_data[j,2]
      
      if(x <= max_x && x >= min_x){
        if(y <= max_y && y >= min_y){
          counter = counter + 1
        }
      }
    }
    min_x = min_x + grid_cell_x
    points_in_cell[i] = counter
  }
  sum(points_in_cell)
  
  probability = points_in_cell/nrow(no_label_data)
  sum(probability)
  output = entropy_calc(probability)
  return(output)
}

