
recurrentPrediction <- function(s,lag,time ){
  
  library("RSNNS")
  lagged_data <- data.frame(s)
  for (i in 1:lag){
    
    aux <- lag(s, i)
    
    lagged_data <- cbind(lagged_data, aux)
    
  }
  
  lagged_data1 <- lagged_data[-(1:lag),]
  
  input <- lagged_data1[,2:ncol(lagged_data1)]
  output <- lagged_data1[,1]
  
  train<- 1:time
  
  fit <- elman(input[train,], output[train], size= c(30,10), learnFuncParams = c(0.01), maxit = 1000)
  
  plotIterativeError(fit)
  
  y <- as.vector(output[-train])
  
  plot(y, type = "l")
  
  pred <- predict(fit, input[train,])
  
  lines(pred, col = "red")
  
  return (pred)
  
  
  
}


