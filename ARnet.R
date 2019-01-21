ARnet <- function(s, lag){
  #S is sorted, so we "de-sort It"
  lagged_data <- data.frame(s)
 
  for (i in 1:lag){
    
    aux <- lag(s, i)
    
    lagged_data <- cbind(lagged_data, aux)
    
  }
  
  
  
  lagged_data <- lagged_data[-(1:lag),]
  
  return(lagged_data)
    
  }